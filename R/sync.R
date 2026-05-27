# eolas_sync() and eolas_sync_all() — multi-file dataset directory sync.
#
# Mirrors the Python sync.py implementation exactly.  Each synced dataset
# lives in a sub-directory of `library_dir`:
#
#   library_dir/
#     doc_huts/
#       snapshot-2026-05-27.parquet
#       delta-2026-05-27-to-2026-06-03.parquet
#       _eolas-manifest.json
#
# Decision logic (same as Python client)
# ---------------------------------------
#   1. Read local manifest.  NULL → first sync → full bulk download.
#   2. GET /v1/datasets/{name} → current_snapshot_id, incremental_supported.
#   3. manifest.current_snapshot == server current_snapshot_id → "unchanged".
#   4. incremental_supported == FALSE → full re-download.
#   5. GET /v1/datasets/{name}/data/incremental?since_snapshot=<id>&format=...
#      410 or 400 → full re-download fallback.
#      200 + X-Eolas-Row-Count: 0 → "unchanged".
#      200 with body → save delta file, update manifest → "snapshot_delta".
#
# Concurrency
# -----------
# eolas_sync_all() uses parallel::mclapply() on Unix-like platforms (mc.cores
# up to max_concurrent) and falls back to lapply() on Windows — R's fork-based
# parallel is unsupported on Windows and causes crashes if called.
#
# The fallback is intentional and documented in the function body.  Pipeline
# users on Windows should run individual eolas_sync() calls or use a background
# process manager.

# ---------------------------------------------------------------------------
# Internal: read a synced dataset from the multi-file library
# ---------------------------------------------------------------------------

# Used by eolas_get_local() when a _eolas-manifest.json is detected.
.eolas_read_from_sync_library <- function(name, library_dir,
                                          as_sf = TRUE, as_arrow = FALSE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "The `arrow` package is required to read synced datasets. ",
      "Install with: install.packages(\"arrow\")",
      call. = FALSE
    )
  }

  dataset_dir <- file.path(library_dir, name)
  if (!dir.exists(dataset_dir)) {
    stop(paste0(
      "eolas: sync library directory not found: ", dataset_dir
    ), call. = FALSE)
  }

  arrow_ds <- tryCatch(
    arrow::open_dataset(dataset_dir, format = "parquet",
                        unify_schemas = TRUE),
    error = function(e) {
      stop(paste0(
        "eolas: failed to open synced dataset '", name, "' from ", dataset_dir,
        ": ", conditionMessage(e)
      ), call. = FALSE)
    }
  )

  # as_arrow=TRUE: return Arrow Dataset without collecting.
  if (isTRUE(as_arrow)) {
    return(arrow_ds)
  }

  # Collect into a data.frame.
  df <- tryCatch(
    as.data.frame(arrow_ds),
    error = function(e) stop(paste0(
      "eolas: failed to read synced dataset '", name, "': ", conditionMessage(e)
    ), call. = FALSE)
  )

  # Optional sf conversion when geometry_wkt is present.
  if (isTRUE(as_sf) && "geometry_wkt" %in% names(df)) {
    df <- tryCatch(
      .eolas_to_sf(df, force = FALSE),
      error = function(e) {
        msg <- conditionMessage(e)
        cli::cli_warn(c(
          "sf conversion failed for {.val {name}}; returning plain {.cls data.frame} with {.field geometry_wkt} column.",
          "x" = "{msg}"
        ))
        df
      }
    )
  }

  df
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.sync_utc_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.sync_today_str <- function() {
  format(Sys.time(), "%Y-%m-%d", tz = "UTC")
}

# Count rows in a parquet file using arrow's parquet reader.
# Returns 0 on any error — row counts are informational only.
.sync_count_parquet_rows <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(0L)
  tryCatch(
    {
      meta <- arrow::read_parquet(path, as_data_frame = FALSE)
      as.integer(meta$num_rows)
    },
    error = function(e) {
      tryCatch(
        {
          pf <- arrow::ParquetFileReader$create(path)
          pf$GetSchema()
          as.integer(pf$ReadTable()$num_rows)
        },
        error = function(e2) 0L
      )
    }
  )
}

# Better row-count approach: use parquet metadata directly.
.sync_parquet_row_count <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(0L)
  tryCatch({
    pq  <- arrow::read_parquet(path, as_data_frame = FALSE)
    as.integer(pq$num_rows)
  }, error = function(e) 0L)
}

# Extract YYYY-MM-DD from the last manifest entry's synced_at field.
.sync_date_from_manifest_tail <- function(manifest) {
  snaps <- manifest$snapshots %||% list()
  if (length(snaps) > 0L) {
    synced_at <- snaps[[length(snaps)]]$synced_at %||% ""
    if (nchar(synced_at) >= 10L) return(substr(synced_at, 1L, 10L))
  }
  .sync_today_str()
}

# Determine the parquet format from dataset metadata.
.sync_detect_format <- function(meta) {
  gt         <- meta$geometry_type
  wkt        <- meta$geometry_wkt
  gt_truthy  <- !is.null(gt)  && nzchar(gt)  && gt  != "none"
  wkt_truthy <- !is.null(wkt) && nzchar(wkt) && wkt != "none"
  is_geo     <- gt_truthy || wkt_truthy || isTRUE(meta$has_geometry)
  if (is_geo) "geoparquet" else "parquet"
}

# Atomic write of raw bytes to `dest_path` via tmp + rename.
# Returns bytes written.
.sync_atomic_write_bytes <- function(dest_path, raw_bytes) {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  rand_hex <- paste0(sample(c(0:9, letters[1:6]), 8L, replace = TRUE),
                     collapse = "")
  tmp_path <- paste0(dest_path, ".eolas-tmp-", rand_hex)
  tryCatch({
    writeBin(raw_bytes, tmp_path)
    ok <- file.rename(tmp_path, dest_path)
    if (!ok) {
      file.copy(tmp_path, dest_path, overwrite = TRUE)
      unlink(tmp_path)
    }
  }, error = function(e) {
    unlink(tmp_path)
    stop(paste0("Failed to write file ", dest_path, ": ",
                conditionMessage(e)), call. = FALSE)
  })
  length(raw_bytes)
}

# Stream an httr2 connection-response to `dest_path` atomically.
# Returns bytes written.
.sync_stream_atomic_write <- function(conn_resp, dest_path, label,
                                      total_bytes = NA, show_bar = FALSE) {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  rand_hex <- paste0(sample(c(0:9, letters[1:6]), 8L, replace = TRUE),
                     collapse = "")
  tmp_path <- paste0(dest_path, ".eolas-tmp-", rand_hex)

  has_cli <- requireNamespace("cli", quietly = TRUE)

  if (show_bar && has_cli) {
    bar_id <- cli::cli_progress_bar(
      name   = label,
      total  = if (is.na(total_bytes)) NA else total_bytes,
      format = paste0(
        "{cli::pb_name} ",
        "{cli::pb_current_bytes}/{cli::pb_total_bytes} ",
        "{cli::pb_rate_bytes} ETA {cli::pb_eta}"
      ),
      clear  = FALSE
    )
    on.exit(cli::cli_progress_done(id = bar_id), add = TRUE)
  }

  bytes_written <- 0L
  CHUNK         <- 1048576L  # 1 MiB

  tryCatch({
    fh <- file(tmp_path, open = "wb")
    on.exit(close(fh), add = TRUE)

    repeat {
      chunk <- httr2::resp_stream_raw(conn_resp, kb = CHUNK %/% 1024L)
      if (length(chunk) == 0L) break
      writeBin(chunk, fh)
      bytes_written <- bytes_written + length(chunk)
      if (show_bar && has_cli) {
        cli::cli_progress_update(inc = length(chunk), id = bar_id)
      }
    }
    close(fh)
    on.exit(NULL, add = FALSE)  # cancel the on.exit close — already done

    ok <- file.rename(tmp_path, dest_path)
    if (!ok) {
      file.copy(tmp_path, dest_path, overwrite = TRUE)
      unlink(tmp_path)
    }
  }, error = function(e) {
    unlink(tmp_path)
    stop(paste0("Stream write failed for ", dest_path, ": ",
                conditionMessage(e)), call. = FALSE)
  })

  bytes_written
}


# ---------------------------------------------------------------------------
# S3 class: eolas_sync_result
# ---------------------------------------------------------------------------

# Constructor — not exported; returned by eolas_sync() internally.
.new_sync_result <- function(status, dataset, library_dir,
                             bytes_downloaded, rows_added, files_added,
                             error = NULL) {
  structure(
    list(
      status           = status,
      dataset          = dataset,
      library_dir      = library_dir,
      bytes_downloaded = bytes_downloaded,
      rows_added       = rows_added,
      files_added      = files_added,
      error            = error
    ),
    class = "eolas_sync_result"
  )
}

#' @export
print.eolas_sync_result <- function(x, ...) {
  if (!is.null(x$error)) {
    cat(sprintf(
      "<eolas_sync_result dataset=%s status=%s error=%s>\n",
      x$dataset, x$status, x$error
    ))
  } else {
    cat(sprintf(
      "<eolas_sync_result dataset=%s status=%s rows_added=%d bytes_downloaded=%d>\n",
      x$dataset, x$status,
      as.integer(x$rows_added),
      as.integer(x$bytes_downloaded)
    ))
  }
  invisible(x)
}


# ---------------------------------------------------------------------------
# Internal: full bulk download (first sync or fallback)
# ---------------------------------------------------------------------------

.sync_do_full_download <- function(name, meta, dataset_dir,
                                   library_dir, fmt, base_url,
                                   show_bar = FALSE) {
  namespace <- meta$namespace %||% ""
  table     <- meta$table %||% meta$name %||% name
  if (!nzchar(namespace)) {
    stop(paste0(
      "Dataset '", name, "' metadata did not include a namespace field. ",
      "Cannot construct bulk URL."
    ), call. = FALSE)
  }

  today        <- .sync_today_str()
  ext          <- if (fmt == "geoparquet") ".geo.parquet" else ".parquet"
  snap_filename <- paste0("snapshot-", today, ext)
  snap_path     <- file.path(dataset_dir, snap_filename)

  key      <- eolas_get_key_internal()
  bulk_url <- paste0(base_url, "/v1/bulk/", namespace, "/", table)
  req <- httr2::request(bulk_url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(format = fmt) |>
    httr2::req_error(is_error = \(r) FALSE)

  use_streaming <- .eolas_use_streaming()
  if (use_streaming) {
    conn_resp <- httr2::req_perform_connection(req)
    status    <- httr2::resp_status(conn_resp)
  } else {
    conn_resp <- eolas_http_perform(req)
    status    <- httr2::resp_status(conn_resp)
  }

  # Bulk status handling (mirrors bulk.R).
  if (status == 402L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% "Fresh bulk downloads are a Pro feature."
    stop("Bulk upgrade required: ", detail, call. = FALSE)
  }
  if (status == 403L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      stop("Bulk licence restricted: ", detail, call. = FALSE)
    }
    eolas_check_status(conn_resp)
  }
  if (status == 503L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% "Monthly bulk snapshots are still rolling out."
    stop("Bulk not yet available: ", detail, call. = FALSE)
  }
  if (status != 200L) {
    eolas_check_status(conn_resp)
  }

  total_bytes <- tryCatch(
    as.numeric(httr2::resp_header(conn_resp, "Content-Length")),
    error = \(e) NA_real_
  )

  if (use_streaming) {
    bytes_dl <- .sync_stream_atomic_write(conn_resp, snap_path,
                                          label = snap_filename,
                                          total_bytes = total_bytes,
                                          show_bar = show_bar)
    tryCatch(close(conn_resp), error = \(e) invisible(NULL))
  } else {
    raw_bytes <- httr2::resp_body_raw(conn_resp)
    bytes_dl  <- .sync_atomic_write_bytes(snap_path, raw_bytes)
  }

  if (bytes_dl == 0L) {
    stop(paste0(
      "Bulk download for '", name, "' returned an empty body (0 bytes). ",
      "The snapshot may not exist for this format. ",
      "Try format='parquet' for non-geo datasets."
    ), call. = FALSE)
  }

  row_count <- .sync_parquet_row_count(snap_path)

  snap_id_raw <- meta$current_snapshot_id
  snap_id     <- if (!is.null(snap_id_raw)) as.numeric(snap_id_raw) else 0

  new_manifest <- list(
    dataset          = name,
    snapshots        = list(list(
      snapshot_id = snap_id,
      kind        = "snapshot",
      file        = snap_filename,
      synced_at   = .sync_utc_now(),
      rows        = as.integer(row_count)
    )),
    current_snapshot = snap_id,
    format           = fmt,
    schema_version   = .MANIFEST_SCHEMA_VERSION
  )
  .eolas_write_manifest(library_dir, name, new_manifest)

  .new_sync_result(
    status           = "snapshot_full",
    dataset          = name,
    library_dir      = library_dir,
    bytes_downloaded = bytes_dl,
    rows_added       = as.integer(row_count),
    files_added      = 1L
  )
}


# ---------------------------------------------------------------------------
# Public: eolas_sync()
# ---------------------------------------------------------------------------

#' Sync a single dataset to a local library directory
#'
#' Downloads or incrementally updates a dataset on disk, following the same
#' decision logic as `client.sync()` in the Python `eolas-data` package.
#' Both clients share the `_eolas-manifest.json` format, so a library synced
#' from Python can be read from R and vice versa.
#'
#' @section Decision logic:
#' \enumerate{
#'   \item Read the local manifest (`NULL` → first sync → full bulk download).
#'   \item Fetch `GET /v1/datasets/{name}` to get `current_snapshot_id` and
#'     `incremental_supported`.
#'   \item If the local snapshot id matches the server → `status = "unchanged"`.
#'   \item If `incremental_supported = FALSE` → full re-download.
#'   \item Attempt `GET /v1/datasets/{name}/data/incremental?since_snapshot=<id>`.
#'     A 410 or 400 falls back to a full re-download.
#'     A 200 with `X-Eolas-Row-Count: 0` returns `"unchanged"`.
#'     A 200 with data saves a delta file → `status = "snapshot_delta"`.
#' }
#'
#' @section Result object:
#' Returns an `eolas_sync_result` list with fields:
#' \describe{
#'   \item{`status`}{`"unchanged"`, `"snapshot_full"`, `"snapshot_delta"`, or `"error"`}
#'   \item{`dataset`}{The dataset name passed in.}
#'   \item{`library_dir`}{Resolved library directory.}
#'   \item{`bytes_downloaded`}{Total bytes written to disk (0 for unchanged).}
#'   \item{`rows_added`}{New rows added in this sync (0 for unchanged).}
#'   \item{`files_added`}{New parquet files written (0 for unchanged).}
#'   \item{`error`}{`NULL` for non-error statuses; error message string for `"error"`.}
#' }
#'
#' @param name Dataset identifier, e.g. `"doc_huts"` or `"nz_parcels"`.
#' @param library_dir Root directory of the local data library.  A
#'   sub-directory named `<name>` is created inside it.  Accepts `~`-prefixed
#'   paths.  When `NULL` (default) the library is resolved via
#'   `eolas_resolve_library_dir()`: `EOLAS_LIBRARY` env var →
#'   `~/.eolas/config.json` → `~/.cache/eolas/`.
#' @param progress Control the download progress bar.  `NULL` (default)
#'   auto-detects: shown in interactive sessions.  `TRUE` forces on; `FALSE`
#'   forces off.  Also suppressed by `EOLAS_NO_PROGRESS=1`.
#' @param base_url Override the API base URL (useful for testing).
#' @return An `eolas_sync_result` S3 object.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # First call: full bulk download
#' r <- eolas_sync("doc_huts", library_dir = "/data/eolas")
#' print(r)
#' # <eolas_sync_result dataset=doc_huts status=snapshot_full rows_added=2500 ...>
#'
#' # Second call: unchanged
#' r2 <- eolas_sync("doc_huts", library_dir = "/data/eolas")
#' print(r2)
#' # <eolas_sync_result dataset=doc_huts status=unchanged rows_added=0 ...>
#' }
eolas_sync <- function(name,
                       library_dir = NULL,
                       progress    = NULL,
                       base_url    = EOLAS_BASE_URL) {

  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }

  # Resolve library_dir.
  if (is.null(library_dir)) {
    lib <- eolas_resolve_library_dir()
  } else {
    lib <- normalizePath(path.expand(as.character(library_dir)), mustWork = FALSE)
  }

  show_bar <- .eolas_resolve_progress(progress)

  # ------------------------------------------------------------------
  # 1. Read local manifest
  # ------------------------------------------------------------------
  manifest <- .eolas_read_manifest(lib, name)

  # ------------------------------------------------------------------
  # 2. Fetch server metadata
  # ------------------------------------------------------------------
  meta <- tryCatch(
    eolas_info(name, base_url = base_url),
    error = function(e) stop(paste0(
      "eolas_sync(", name, "): cannot fetch metadata: ", conditionMessage(e)
    ), call. = FALSE)
  )

  current_snapshot_id_raw <- meta$current_snapshot_id
  current_snapshot_id <- if (!is.null(current_snapshot_id_raw)) {
    as.numeric(current_snapshot_id_raw)
  } else {
    NULL
  }

  incremental_supported <- isTRUE(meta$incremental_supported %||% TRUE)
  fmt <- .sync_detect_format(meta)
  dataset_dir <- file.path(lib, name)

  # ------------------------------------------------------------------
  # 3. No local manifest → first sync (full bulk download)
  # ------------------------------------------------------------------
  if (is.null(manifest)) {
    return(.sync_do_full_download(
      name        = name,
      meta        = meta,
      dataset_dir = dataset_dir,
      library_dir = lib,
      fmt         = fmt,
      base_url    = base_url,
      show_bar    = show_bar
    ))
  }

  # ------------------------------------------------------------------
  # 4. Server didn't return snapshot id → conservative full download
  # ------------------------------------------------------------------
  if (is.null(current_snapshot_id)) {
    cli::cli_alert_info("{.fn eolas_sync}({.val {name}}): server did not return current_snapshot_id; falling back to full download.")
    return(.sync_do_full_download(
      name        = name,
      meta        = meta,
      dataset_dir = dataset_dir,
      library_dir = lib,
      fmt         = fmt,
      base_url    = base_url,
      show_bar    = show_bar
    ))
  }

  # ------------------------------------------------------------------
  # 5. Snapshot unchanged → no-op
  # Use string comparison to avoid precision loss on 64-bit Iceberg snapshot
  # ids, which exceed R's double precision (2^53).  The manifest stores
  # snapshot ids as strings (new R format) or numeric (Python/old R format);
  # normalise both to character for a lossless comparison.
  # ------------------------------------------------------------------
  local_snap_chr  <- as.character(manifest$current_snapshot %||% "")
  server_snap_chr <- as.character(current_snapshot_id %||% "")
  if (nzchar(local_snap_chr) && nzchar(server_snap_chr) &&
      local_snap_chr == server_snap_chr) {
    return(.new_sync_result(
      status           = "unchanged",
      dataset          = name,
      library_dir      = lib,
      bytes_downloaded = 0L,
      rows_added       = 0L,
      files_added      = 0L
    ))
  }

  # ------------------------------------------------------------------
  # 6. incremental_supported = FALSE → full re-download
  # ------------------------------------------------------------------
  if (!incremental_supported) {
    return(.sync_do_full_download(
      name        = name,
      meta        = meta,
      dataset_dir = dataset_dir,
      library_dir = lib,
      fmt         = fmt,
      base_url    = base_url,
      show_bar    = show_bar
    ))
  }

  # ------------------------------------------------------------------
  # 7. Attempt incremental delta
  # ------------------------------------------------------------------
  # since_id must be the exact integer string for the API query param.
  # Use the string form from the manifest if available (preserves full
  # 64-bit precision); fall back to numeric conversion otherwise.
  since_id <- as.character(manifest$current_snapshot)
  key      <- eolas_get_key_internal()

  incremental_url <- paste0(base_url, "/v1/datasets/", name, "/data/incremental")
  req <- httr2::request(incremental_url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(since_snapshot = since_id, format = fmt) |>
    httr2::req_error(is_error = \(r) FALSE)

  use_streaming <- .eolas_use_streaming()
  if (use_streaming) {
    conn_resp <- httr2::req_perform_connection(req)
    inc_status <- httr2::resp_status(conn_resp)
  } else {
    conn_resp  <- eolas_http_perform(req)
    inc_status <- httr2::resp_status(conn_resp)
  }

  # 410 or 400 → lineage broken / incremental_supported false → full download
  if (inc_status %in% c(400L, 410L)) {
    if (use_streaming) tryCatch(close(conn_resp), error = \(e) invisible(NULL))
    return(.sync_do_full_download(
      name        = name,
      meta        = meta,
      dataset_dir = dataset_dir,
      library_dir = lib,
      fmt         = fmt,
      base_url    = base_url,
      show_bar    = show_bar
    ))
  }

  # Any other non-200 → propagate error
  if (inc_status != 200L) {
    eolas_check_status(conn_resp)
  }

  # Check row count header — 0 rows means since == current (edge case)
  row_count_hdr <- tryCatch(
    httr2::resp_header(conn_resp, "X-Eolas-Row-Count"),
    error = \(e) NULL
  )
  server_row_count <- tryCatch(
    as.integer(row_count_hdr),
    warning = \(e) NULL,
    error   = \(e) NULL
  )

  if (!is.null(server_row_count) && !is.na(server_row_count) &&
      server_row_count == 0L) {
    if (use_streaming) tryCatch(close(conn_resp), error = \(e) invisible(NULL))
    return(.new_sync_result(
      status           = "unchanged",
      dataset          = name,
      library_dir      = lib,
      bytes_downloaded = 0L,
      rows_added       = 0L,
      files_added      = 0L
    ))
  }

  # ------------------------------------------------------------------
  # 8. Save the delta file
  # ------------------------------------------------------------------
  from_date    <- .sync_date_from_manifest_tail(manifest)
  to_date      <- .sync_today_str()
  ext          <- if (fmt == "geoparquet") ".geo.parquet" else ".parquet"
  delta_filename <- paste0("delta-", from_date, "-to-", to_date, ext)
  delta_path     <- file.path(dataset_dir, delta_filename)

  total_bytes <- tryCatch(
    as.numeric(httr2::resp_header(conn_resp, "Content-Length")),
    error = \(e) NA_real_
  )

  if (use_streaming) {
    bytes_dl <- .sync_stream_atomic_write(conn_resp, delta_path,
                                          label = delta_filename,
                                          total_bytes = total_bytes,
                                          show_bar = show_bar)
    tryCatch(close(conn_resp), error = \(e) invisible(NULL))
  } else {
    raw_bytes <- httr2::resp_body_raw(conn_resp)
    bytes_dl  <- .sync_atomic_write_bytes(delta_path, raw_bytes)
  }

  # If header was absent, count from file.
  if (is.null(server_row_count) || is.na(server_row_count)) {
    server_row_count <- .sync_parquet_row_count(delta_path)
  }

  # ------------------------------------------------------------------
  # 9. Update manifest — append delta entry
  # ------------------------------------------------------------------
  new_entry <- list(
    snapshot_id     = current_snapshot_id,
    kind            = "delta",
    parent_snapshot = since_id,
    file            = delta_filename,
    synced_at       = .sync_utc_now(),
    rows_added      = as.integer(server_row_count)
  )
  manifest$snapshots <- c(manifest$snapshots, list(new_entry))
  manifest$current_snapshot <- current_snapshot_id
  .eolas_write_manifest(lib, name, manifest)

  .new_sync_result(
    status           = "snapshot_delta",
    dataset          = name,
    library_dir      = lib,
    bytes_downloaded = bytes_dl,
    rows_added       = as.integer(server_row_count),
    files_added      = 1L
  )
}


# ---------------------------------------------------------------------------
# Public: eolas_sync_all()
# ---------------------------------------------------------------------------

#' Sync multiple datasets to a local library directory
#'
#' Calls [eolas_sync()] for each dataset, optionally in parallel.  When
#' `datasets = NULL` all sub-directories that contain a `_eolas-manifest.json`
#' are discovered and synced automatically.
#'
#' @section Parallelism:
#' Uses `parallel::mclapply()` on Unix-like systems (fork-based, up to
#' `max_concurrent` cores).  **On Windows**, `mclapply()` is not supported;
#' the function falls back to sequential `lapply()` automatically with a
#' one-time message.  Windows pipeline users should wrap individual
#' `eolas_sync()` calls in a `future` or background process if concurrency is
#' needed.
#'
#' @param library_dir Root directory of the local data library.  When `NULL`
#'   the library is resolved via `eolas_resolve_library_dir()`.
#' @param datasets Character vector of dataset names to sync.  When `NULL`
#'   (default) all sub-directories with a `_eolas-manifest.json` are synced.
#' @param max_concurrent Maximum parallel sync operations (default `4`).
#'   Ignored on Windows (see Parallelism section).
#' @param progress Passed to each [eolas_sync()] call.
#' @param base_url Override the API base URL (useful for testing).
#' @return A named list of `eolas_sync_result` objects, one per dataset.
#'   On a per-dataset failure the corresponding entry has `status = "error"`
#'   and `error` set to the error message; other datasets still complete
#'   normally.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # Sync specific datasets
#' results <- eolas_sync_all(
#'   library_dir = "/data/eolas",
#'   datasets    = c("doc_huts", "nz_cpi")
#' )
#'
#' # Auto-discover all synced datasets and refresh them
#' results <- eolas_sync_all(library_dir = "/data/eolas")
#'
#' # Check results
#' for (r in results) print(r)
#' }
eolas_sync_all <- function(library_dir  = NULL,
                           datasets     = NULL,
                           max_concurrent = 4L,
                           progress       = NULL,
                           base_url       = EOLAS_BASE_URL) {

  # Resolve library_dir.
  if (is.null(library_dir)) {
    lib <- eolas_resolve_library_dir()
  } else {
    lib <- normalizePath(path.expand(as.character(library_dir)), mustWork = FALSE)
  }

  # Discover or use explicit dataset list.
  if (is.null(datasets)) {
    names_to_sync <- character(0L)
    if (dir.exists(lib)) {
      subdirs <- list.dirs(lib, full.names = FALSE, recursive = FALSE)
      for (d in sort(subdirs)) {
        if (file.exists(file.path(lib, d, .MANIFEST_FILENAME))) {
          names_to_sync <- c(names_to_sync, d)
        }
      }
    }
    if (length(names_to_sync) == 0L) return(list())
  } else {
    names_to_sync <- as.character(datasets)
  }

  total <- length(names_to_sync)
  idx   <- seq_len(total)

  # Worker function — wraps eolas_sync() and catches per-dataset errors.
  .sync_one <- function(i) {
    nm <- names_to_sync[[i]]
    tryCatch(
      eolas_sync(nm, library_dir = lib, progress = progress, base_url = base_url),
      error = function(e) {
        .new_sync_result(
          status           = "error",
          dataset          = nm,
          library_dir      = lib,
          bytes_downloaded = 0L,
          rows_added       = 0L,
          files_added      = 0L,
          error            = conditionMessage(e)
        )
      }
    )
  }

  # Parallel on non-Windows; sequential fallback on Windows.
  results_list <- if (.Platform$OS.type != "windows" &&
                       max_concurrent > 1L && total > 1L &&
                       requireNamespace("parallel", quietly = TRUE)) {
    parallel::mclapply(idx, .sync_one, mc.cores = min(max_concurrent, total))
  } else {
    if (.Platform$OS.type == "windows" && max_concurrent > 1L) {
      cli::cli_alert_info(c(
        "{.fn parallel::mclapply} is not supported on Windows.",
        "i" = "Running {.fn eolas_sync_all} sequentially."
      ))
    }
    lapply(idx, .sync_one)
  }

  stats::setNames(results_list, names_to_sync)
}
