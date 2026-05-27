# eolas_compact() — merge all parquet files in a dataset directory into one.
#
# Mirrors the Python compact.py implementation exactly.  Uses the same
# atomicity strategy:
#
#   1. Read all parquet files via arrow::open_dataset() and write the merged
#      table to .compacting-<uuid>/snapshot-<today>.parquet.tmp.
#   2. Rename .parquet.tmp → .parquet (still in the staging dir).
#   3. Rename .compacting-<uuid> → .compacting-done-<uuid>  (checkpoint).
#   4. Write new manifest via .eolas_write_manifest() (tmp → rename).
#   5. Move merged snapshot up to the dataset directory.
#   6. Delete old (now-orphaned) snapshot + delta files.
#   7. Delete the .compacting-done-<uuid> staging directory.
#
# A crashed compact leaves either the original state intact (steps 1-3 not
# complete) or a .compacting-done-<uuid> dir (steps 4-6 not reached).  The
# next compact run detects and cleans up stale staging directories.
#
# SCD2 tables are NOT collapsed — all historical rows are preserved.
# Row-level deduplication is explicitly out of scope for v1.


# ---------------------------------------------------------------------------
# S3 class: eolas_compact_result
# ---------------------------------------------------------------------------

.new_compact_result <- function(dataset, rows_before, rows_after,
                                files_before, files_after, bytes_saved) {
  structure(
    list(
      dataset      = dataset,
      rows_before  = rows_before,
      rows_after   = rows_after,
      files_before = files_before,
      files_after  = files_after,
      bytes_saved  = bytes_saved
    ),
    class = "eolas_compact_result"
  )
}

#' @export
print.eolas_compact_result <- function(x, ...) {
  cat(sprintf(
    "<eolas_compact_result dataset=%s files=%d->%d rows=%d bytes_saved=%d>\n",
    x$dataset,
    as.integer(x$files_before), as.integer(x$files_after),
    as.integer(x$rows_before),  as.integer(x$bytes_saved)
  ))
  invisible(x)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.compact_today_str <- function() {
  format(Sys.time(), "%Y-%m-%d", tz = "UTC")
}

.compact_utc_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# List all snapshot/delta parquet files in dataset_dir (sorted).
.compact_list_data_files <- function(dataset_dir) {
  all_files <- list.files(dataset_dir, full.names = TRUE, recursive = FALSE)
  keep <- vapply(all_files, function(f) {
    nm <- basename(f)
    is_file <- !dir.exists(f)
    is_data <- (startsWith(nm, "snapshot-") || startsWith(nm, "delta-")) &&
               (endsWith(nm, ".parquet") || endsWith(nm, ".geo.parquet"))
    is_file && is_data
  }, logical(1L))
  sort(all_files[keep])
}

# Count parquet rows — cheap via Arrow metadata.
.compact_parquet_row_count <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(0L)
  tryCatch(
    as.integer(arrow::read_parquet(path, as_data_frame = FALSE)$num_rows),
    error = function(e) 0L
  )
}

# Remove any leftover .compacting-* / .compacting-done-* staging dirs.
.compact_cleanup_stale_staging <- function(dataset_dir) {
  entries <- list.dirs(dataset_dir, full.names = TRUE, recursive = FALSE)
  for (d in entries) {
    nm <- basename(d)
    if (startsWith(nm, ".compacting-done-") || startsWith(nm, ".compacting-")) {
      unlink(d, recursive = TRUE)
    }
  }
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# Implementation: .compact_one_dataset()
# ---------------------------------------------------------------------------

.compact_one_dataset <- function(dataset_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "The `arrow` package is required for eolas_compact(). ",
      "Install with: install.packages(\"arrow\")",
      call. = FALSE
    )
  }

  ddir <- normalizePath(path.expand(dataset_dir), mustWork = FALSE)
  if (!dir.exists(ddir)) {
    stop(paste0("eolas_compact(): dataset_dir does not exist: ", ddir),
         call. = FALSE)
  }

  manifest_path <- file.path(ddir, .MANIFEST_FILENAME)
  manifest      <- .eolas_read_manifest(dirname(ddir), basename(ddir))
  if (is.null(manifest)) {
    stop(paste0(
      "eolas_compact(): no manifest found at ", manifest_path, ". ",
      "Has this dataset been synced yet?"
    ), call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 0. Clean up stale staging dirs from previous crashed runs
  # ------------------------------------------------------------------
  .compact_cleanup_stale_staging(ddir)

  # ------------------------------------------------------------------
  # 1. Enumerate data files and measure
  # ------------------------------------------------------------------
  data_files   <- .compact_list_data_files(ddir)
  files_before <- length(data_files)

  if (files_before == 0L) {
    return(.new_compact_result(
      dataset      = manifest$dataset,
      rows_before  = 0L,
      rows_after   = 0L,
      files_before = 0L,
      files_after  = 0L,
      bytes_saved  = 0L
    ))
  }

  if (files_before == 1L) {
    rows <- .compact_parquet_row_count(data_files[[1L]])
    return(.new_compact_result(
      dataset      = manifest$dataset,
      rows_before  = rows,
      rows_after   = rows,
      files_before = 1L,
      files_after  = 1L,
      bytes_saved  = 0L
    ))
  }

  old_total_bytes <- sum(vapply(data_files, function(f) file.info(f)$size, numeric(1L)),
                         na.rm = TRUE)
  rows_before <- sum(vapply(data_files, .compact_parquet_row_count, integer(1L)))

  # ------------------------------------------------------------------
  # 2. Determine output format from manifest
  # ------------------------------------------------------------------
  fmt          <- manifest$format %||% "parquet"
  ext          <- if (fmt == "geoparquet") ".geo.parquet" else ".parquet"
  today        <- .compact_today_str()
  new_filename <- paste0("snapshot-", today, ext)

  # ------------------------------------------------------------------
  # 3. Create staging directory
  # ------------------------------------------------------------------
  uid         <- paste0(sample(c(0:9, letters[1:6]), 12L, replace = TRUE),
                        collapse = "")
  staging_dir <- file.path(ddir, paste0(".compacting-", uid))
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  new_file_tmp    <- file.path(staging_dir, paste0(new_filename, ".tmp"))
  new_file_staged <- file.path(staging_dir, new_filename)

  tryCatch({
    # --------------------------------------------------------------
    # 4. Read all files via arrow::open_dataset + write merged table
    # --------------------------------------------------------------
    arrow_ds    <- arrow::open_dataset(data_files, format = "parquet",
                                       unify_schemas = TRUE)
    merged_tbl  <- as.data.frame(arrow_ds)
    rows_after  <- nrow(merged_tbl)

    arrow::write_parquet(merged_tbl, new_file_tmp)
    rm(merged_tbl)
    gc(verbose = FALSE)

    # Rename .parquet.tmp → .parquet within staging dir.
    ok <- file.rename(new_file_tmp, new_file_staged)
    if (!ok) {
      file.copy(new_file_tmp, new_file_staged, overwrite = TRUE)
      unlink(new_file_tmp)
    }
  }, error = function(e) {
    unlink(staging_dir, recursive = TRUE)
    stop(paste0("eolas_compact(): merge failed: ", conditionMessage(e)),
         call. = FALSE)
  })

  # ------------------------------------------------------------------
  # 5. Checkpoint: rename .compacting-<uid> → .compacting-done-<uid>
  # ------------------------------------------------------------------
  done_dir <- file.path(ddir, paste0(".compacting-done-", uid))
  ok_ck <- file.rename(staging_dir, done_dir)
  if (!ok_ck) {
    # Rare (same filesystem required); clean up and abort.
    unlink(staging_dir, recursive = TRUE)
    stop("eolas_compact(): checkpoint rename failed.", call. = FALSE)
  }
  new_file_in_done <- file.path(done_dir, new_filename)

  # From here, original state (manifest + old files) is still intact.

  # ------------------------------------------------------------------
  # 6. Write new manifest
  # ------------------------------------------------------------------
  new_snap_id <- as.numeric(manifest$current_snapshot %||% 0)
  new_manifest <- list(
    dataset   = manifest$dataset,
    snapshots = list(list(
      snapshot_id = new_snap_id,
      kind        = "snapshot",
      file        = new_filename,
      synced_at   = .compact_utc_now(),
      rows        = as.integer(rows_after)
    )),
    current_snapshot = new_snap_id,
    format           = fmt,
    schema_version   = .MANIFEST_SCHEMA_VERSION
  )
  .eolas_write_manifest(dirname(ddir), basename(ddir), new_manifest)

  # ------------------------------------------------------------------
  # 7. Move merged snapshot from done_dir up to dataset_dir
  # ------------------------------------------------------------------
  final_path <- file.path(ddir, new_filename)
  ok_mv <- file.rename(new_file_in_done, final_path)
  if (!ok_mv) {
    file.copy(new_file_in_done, final_path, overwrite = TRUE)
    unlink(new_file_in_done)
  }

  # ------------------------------------------------------------------
  # 8. Delete old orphaned files
  #    Guard: skip the newly-written merged file even if its name
  #    collides with an existing snapshot (same-date edge case).
  # ------------------------------------------------------------------
  final_norm <- normalizePath(final_path, mustWork = FALSE)
  for (old_f in data_files) {
    if (normalizePath(old_f, mustWork = FALSE) == final_norm) next
    tryCatch(unlink(old_f), error = \(e) invisible(NULL))
  }

  # ------------------------------------------------------------------
  # 9. Delete the staging done dir
  # ------------------------------------------------------------------
  unlink(done_dir, recursive = TRUE)

  new_size    <- tryCatch(file.info(final_path)$size, error = \(e) 0)
  bytes_saved <- as.integer(old_total_bytes - (new_size %||% 0))

  .new_compact_result(
    dataset      = manifest$dataset,
    rows_before  = as.integer(rows_before),
    rows_after   = as.integer(rows_after),
    files_before = files_before,
    files_after  = 1L,
    bytes_saved  = bytes_saved
  )
}


# ---------------------------------------------------------------------------
# Public: eolas_compact()
# ---------------------------------------------------------------------------

#' Compact a synced dataset directory by merging all parquet files into one
#'
#' Reads all snapshot and delta parquet files in the dataset directory via
#' `arrow::open_dataset()`, merges them into a single snapshot file, updates
#' the manifest, and deletes the now-superseded files.
#'
#' The merge is a pure concatenation — SCD2 tables that carry `is_current` /
#' `valid_to` columns are **not** collapsed.  The compacted file contains all
#' historical rows; readers that want current-state filter by
#' `is_current == TRUE` as usual.
#'
#' @section Atomicity:
#' A crashed compact at any step leaves either the original state intact or a
#' clearly-named `.compacting-done-<uuid>` directory.  The next compact run
#' detects and removes stale staging directories automatically.
#'
#' @section One dataset vs all:
#' Pass a path directly to `dataset_dir` to compact one dataset, or pass
#' `library_dir` (optionally with `dataset`) to compact one or all synced
#' datasets under a library root.
#'
#' @param dataset_dir Path to a specific dataset directory
#'   (e.g. `/data/eolas/nz_parcels`).  When set, `library_dir` and
#'   `dataset` are ignored.
#' @param library_dir Root library directory.  When `dataset` is set, compact
#'   only that one dataset; when `dataset = NULL` compact all synced datasets
#'   under the library root.
#' @param dataset Dataset name to compact within `library_dir`.  `NULL`
#'   (default) compacts all datasets.
#' @return An `eolas_compact_result` S3 object (single dataset), or a named
#'   list of `eolas_compact_result` objects when compacting all datasets.
#' @export
#' @examples
#' \dontrun{
#' # Compact a specific dataset directory
#' r <- eolas_compact("/data/eolas/nz_parcels")
#' print(r)
#' # <eolas_compact_result dataset=nz_parcels files=5->1 rows=5431319 ...>
#'
#' # Compact one dataset by name under a library root
#' r <- eolas_compact(library_dir = "/data/eolas", dataset = "nz_parcels")
#'
#' # Compact all synced datasets under a library root
#' results <- eolas_compact(library_dir = "/data/eolas")
#' }
eolas_compact <- function(dataset_dir = NULL,
                          library_dir = NULL,
                          dataset     = NULL) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "The `arrow` package is required for eolas_compact(). ",
      "Install with: install.packages(\"arrow\")",
      call. = FALSE
    )
  }

  # Case 1: explicit dataset_dir
  if (!is.null(dataset_dir)) {
    ddir <- normalizePath(path.expand(as.character(dataset_dir)), mustWork = FALSE)
    return(.compact_one_dataset(ddir))
  }

  # Case 2: library_dir required
  if (is.null(library_dir)) {
    stop(
      "Provide either `dataset_dir` or `library_dir` (optionally with `dataset`).",
      call. = FALSE
    )
  }

  lib <- normalizePath(path.expand(as.character(library_dir)), mustWork = FALSE)

  # Case 2a: one named dataset
  if (!is.null(dataset)) {
    ddir <- file.path(lib, as.character(dataset))
    return(.compact_one_dataset(ddir))
  }

  # Case 2b: all synced datasets under library_dir
  if (!dir.exists(lib)) {
    stop(paste0("eolas_compact(): library_dir does not exist: ", lib), call. = FALSE)
  }
  subdirs <- list.dirs(lib, full.names = TRUE, recursive = FALSE)
  synced <- Filter(function(d) {
    file.exists(file.path(d, .MANIFEST_FILENAME))
  }, subdirs)

  if (length(synced) == 0L) {
    cli::cli_alert_info("{.fn eolas_compact}: no synced datasets found in {.path {lib}}")
    return(invisible(list()))
  }

  results <- lapply(synced, function(d) {
    tryCatch(
      .compact_one_dataset(d),
      error = function(e) {
        dataset <- basename(d)
        msg <- conditionMessage(e)
        cli::cli_warn("{.fn eolas_compact}: compact failed for {.val {dataset}}: {msg}")
        NULL
      }
    )
  })
  names(results) <- basename(synced)
  results
}
