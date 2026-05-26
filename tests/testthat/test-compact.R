library(testthat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Write a minimal-but-valid parquet file to a path.
.write_test_parquet <- function(path, rows = 5L) {
  arrow::write_parquet(
    data.frame(x = seq_len(rows), y = rep("a", rows)),
    path
  )
  invisible(path)
}

# Create a synced dataset directory with one or more parquet files.
.setup_dataset_dir <- function(base_dir, name, snap_id = 1001,
                               files = list(
                                 list(name = "snapshot-2026-05-27.parquet",
                                      kind = "snapshot", rows = 10L)
                               )) {
  ddir <- file.path(base_dir, name)
  dir.create(ddir, recursive = TRUE, showWarnings = FALSE)

  snapshots_list <- vector("list", length(files))
  for (i in seq_along(files)) {
    fi     <- files[[i]]
    fpath  <- file.path(ddir, fi$name)
    row_n  <- fi$rows %||% 5L
    .write_test_parquet(fpath, rows = row_n)

    if (fi$kind == "snapshot") {
      snapshots_list[[i]] <- list(
        snapshot_id = snap_id,
        kind        = "snapshot",
        file        = fi$name,
        synced_at   = "2026-05-27T10:00:00Z",
        rows        = as.integer(row_n)
      )
    } else {
      # Compute a valid date by adding i weeks from a base date
      base_dt  <- as.Date("2026-05-27")
      delta_dt <- format(base_dt + (i * 7L), "%Y-%m-%d")
      snapshots_list[[i]] <- list(
        snapshot_id     = snap_id + i,
        kind            = "delta",
        parent_snapshot = snap_id,
        file            = fi$name,
        synced_at       = paste0(delta_dt, "T10:00:00Z"),
        rows_added      = as.integer(row_n)
      )
    }
  }

  manifest <- list(
    dataset          = name,
    snapshots        = snapshots_list,
    current_snapshot = snap_id,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(base_dir, name, manifest)
  ddir
}


# ===========================================================================
# eolas_compact() — single-file no-op
# ===========================================================================

test_that("compact: single file returns no-op result", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  ddir <- .setup_dataset_dir(td, "doc_huts", files = list(
    list(name = "snapshot-2026-05-27.parquet", kind = "snapshot", rows = 10L)
  ))

  r <- eolas_compact(dataset_dir = ddir)
  expect_s3_class(r, "eolas_compact_result")
  expect_equal(r$files_before, 1L)
  expect_equal(r$files_after,  1L)
  expect_equal(r$bytes_saved,  0L)
  # No change to the directory contents
  parquet_files <- list.files(ddir, pattern = "\\.parquet$")
  expect_equal(length(parquet_files), 1L)
})

test_that("compact: zero files returns no-op result", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  ddir <- file.path(td, "empty_dataset")
  dir.create(ddir)
  manifest <- list(
    dataset          = "empty_dataset",
    snapshots        = list(),
    current_snapshot = NULL,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "empty_dataset", manifest)

  r <- eolas_compact(dataset_dir = ddir)
  expect_equal(r$files_before, 0L)
  expect_equal(r$files_after,  0L)
})


# ===========================================================================
# eolas_compact() — multi-file rollup
# ===========================================================================

test_that("compact: 3 files merged into 1", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  ddir <- .setup_dataset_dir(td, "doc_huts",
    files = list(
      list(name = "snapshot-2026-05-01.parquet", kind = "snapshot", rows = 5L),
      list(name = "delta-2026-05-01-to-2026-05-08.parquet",  kind = "delta",    rows = 3L),
      list(name = "delta-2026-05-08-to-2026-05-15.parquet",  kind = "delta",    rows = 2L)
    )
  )

  r <- eolas_compact(dataset_dir = ddir)
  expect_equal(r$files_before, 3L)
  expect_equal(r$files_after,  1L)
  expect_equal(r$rows_before,  10L)
  expect_equal(r$rows_after,   10L)

  # Only 1 data file left
  data_files <- list.files(ddir,
    pattern = "^(snapshot|delta)-.*\\.parquet$",
    full.names = FALSE)
  expect_equal(length(data_files), 1L)
  expect_true(startsWith(data_files[[1L]], "snapshot-"))

  # Manifest updated to single entry
  m <- .eolas_read_manifest(td, "doc_huts")
  expect_equal(length(m$snapshots), 1L)
  expect_equal(m$snapshots[[1L]]$kind, "snapshot")
})

test_that("compact: manifest current_snapshot preserved after merge", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  .setup_dataset_dir(td, "doc_huts", snap_id = 9999L,
    files = list(
      list(name = "snapshot-2026-05-01.parquet", kind = "snapshot", rows = 4L),
      list(name = "delta-2026-05-01-to-2026-05-08.parquet",  kind = "delta",    rows = 2L)
    )
  )

  eolas_compact(dataset_dir = file.path(td, "doc_huts"))
  m <- .eolas_read_manifest(td, "doc_huts")
  expect_equal(as.numeric(m$current_snapshot), 9999)
})

test_that("compact: no stale .compacting dirs left on success", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  ddir <- .setup_dataset_dir(td, "doc_huts",
    files = list(
      list(name = "snapshot-2026-05-01.parquet", kind = "snapshot", rows = 3L),
      list(name = "delta-2026-05-01-to-2026-05-08.parquet",  kind = "delta",    rows = 2L)
    )
  )
  eolas_compact(dataset_dir = ddir)
  staging <- list.dirs(ddir, full.names = FALSE, recursive = FALSE)
  staging <- staging[startsWith(staging, ".compacting")]
  expect_equal(length(staging), 0L)
})


# ===========================================================================
# eolas_compact() — same-date collision guard
# ===========================================================================

test_that("compact: same-date snapshot not deleted on merge", {
  skip_if_not_installed("arrow")
  td  <- withr::local_tempdir()
  today_name <- paste0("snapshot-", format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
                       ".parquet")
  ddir <- .setup_dataset_dir(td, "doc_huts",
    files = list(
      list(name = today_name,
           kind = "snapshot", rows = 3L),
      list(name = "delta-2026-05-01-to-2026-05-08.parquet",
           kind = "delta", rows = 2L)
    )
  )
  r <- eolas_compact(dataset_dir = ddir)
  # The merged output IS today_name.  It must still exist.
  expect_true(file.exists(file.path(ddir, today_name)))
  expect_equal(r$files_after, 1L)
})


# ===========================================================================
# eolas_compact() — library-level dispatch
# ===========================================================================

test_that("compact: library_dir + dataset compacts one dataset", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  .setup_dataset_dir(td, "alpha",
    files = list(
      list(name = "snapshot-2026-05-01.parquet", kind = "snapshot", rows = 3L),
      list(name = "delta-2026-05-01-to-2026-05-08.parquet",  kind = "delta",    rows = 2L)
    )
  )
  r <- eolas_compact(library_dir = td, dataset = "alpha")
  expect_s3_class(r, "eolas_compact_result")
  expect_equal(r$files_after, 1L)
})

test_that("compact: library_dir without dataset compacts all datasets", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  for (nm in c("ds1", "ds2")) {
    .setup_dataset_dir(td, nm,
      files = list(
        list(name = "snapshot-2026-05-01.parquet", kind = "snapshot", rows = 4L),
        list(name = "delta-2026-05-01-to-2026-05-08.parquet",  kind = "delta",    rows = 1L)
      )
    )
  }
  results <- eolas_compact(library_dir = td)
  expect_equal(length(results), 2L)
  for (r in results) {
    expect_equal(r$files_after, 1L)
  }
})

test_that("compact: errors when no manifest present", {
  skip_if_not_installed("arrow")
  td   <- withr::local_tempdir()
  ddir <- file.path(td, "no_manifest")
  dir.create(ddir)
  expect_error(eolas_compact(dataset_dir = ddir), "no manifest")
})

test_that("compact: errors when dataset_dir does not exist", {
  skip_if_not_installed("arrow")
  expect_error(eolas_compact(dataset_dir = "/nonexistent/path/xyz"),
               "does not exist")
})

test_that("print.eolas_compact_result renders correctly", {
  r <- .new_compact_result("doc_huts", 10L, 10L, 3L, 1L, 5000L)
  out <- capture.output(print(r))
  expect_true(grepl("doc_huts", out))
  expect_true(grepl("3", out))
  expect_true(grepl("1", out))
})
