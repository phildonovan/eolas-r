library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Tests for eolas_get() smart-routing when a sync library is present
# ---------------------------------------------------------------------------
#
# The sync library path is triggered when:
#   - cache_dir is NULL (no explicit override)
#   - EOLAS_LIBRARY env var points to a dir with _eolas-manifest.json/<name>/
#
# It reads from arrow::open_dataset(library_dir/name/).
# mode="live" must bypass the library and hit the live API.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.write_test_parquet <- function(path, rows = 5L) {
  arrow::write_parquet(
    data.frame(x = seq_len(rows), y = rep("a", rows)),
    path
  )
  invisible(path)
}

.setup_sync_library <- function(td, name, rows = 5L, snap_id = 1001L) {
  ddir <- file.path(td, name)
  dir.create(ddir, recursive = TRUE, showWarnings = FALSE)
  fpath <- file.path(ddir, "snapshot-2026-05-27.parquet")
  .write_test_parquet(fpath, rows = rows)

  manifest <- list(
    dataset          = name,
    snapshots        = list(list(
      snapshot_id = snap_id,
      kind        = "snapshot",
      file        = "snapshot-2026-05-27.parquet",
      synced_at   = "2026-05-27T10:00:00Z",
      rows        = as.integer(rows)
    )),
    current_snapshot = snap_id,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, name, manifest)
  invisible(ddir)
}

.fake_resp_lib <- function(body, status = 200L,
                            content_type = "application/json") {
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = as.integer(status),
      headers     = structure(
        list(`content-type` = content_type),
        class = "httr2_headers"
      ),
      body  = if (is.character(body)) charToRaw(body) else body,
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# Live-API mock metadata body for mode="live" tests.
DATA_BODY_LIB <- '{"data":[
  {"date":"2026-05-01","value":42},
  {"date":"2026-05-02","value":43}
]}'


# ===========================================================================
# eolas_get_local() — sync library fast path
# ===========================================================================

test_that("eolas_get_local: reads from sync library when manifest present", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  .setup_sync_library(td, "doc_huts", rows = 7L)

  # Point EOLAS_LIBRARY at our temp dir
  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    df <- eolas_get_local("doc_huts", progress = FALSE)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 7L)
  })
})

test_that("eolas_get_local: returns arrow::Dataset when as_arrow=TRUE with sync library", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  .setup_sync_library(td, "doc_huts", rows = 5L)

  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    ds <- eolas_get_local("doc_huts", as_arrow = TRUE, progress = FALSE)
    # Should be an Arrow Dataset (not a collected data.frame)
    expect_true(
      inherits(ds, "Dataset") || inherits(ds, "ArrowTabular") ||
      inherits(ds, "Table")   || inherits(ds, "FileSystemDataset"),
      info = paste("Got class:", paste(class(ds), collapse = ", "))
    )
  })
})

test_that("eolas_get_local: explicit cache_dir= bypasses sync library", {
  # Verify that when cache_dir is explicitly set, the manifest check is
  # skipped (even if EOLAS_LIBRARY points to a dir with a manifest).
  # We verify this by reading a different dataset altogether so there is
  # no manifest for "nz_cpi" in td (which has only "doc_huts").
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  .setup_sync_library(td, "doc_huts", rows = 7L)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  # Source parquet lives in td (the sync library dir, not the cache dir).
  # This ensures the file.copy() in the mock copies FROM td TO td2,
  # never from-and-to the same path.
  src_parquet <- file.path(td, "nz_cpi_source.parquet")
  arrow::write_parquet(data.frame(x = 1:3, y = "b"), src_parquet)

  # td2 is the explicit cache_dir — destination for the bulk path.
  td2 <- withr::local_tempdir()

  # Mock eolas_info + eolas_sync_bulk to bypass actual HTTP
  meta_nz_cpi <- list(name = "nz_cpi", namespace = "statsnz", table = "nz_cpi",
                       geometry_type = NULL, geometry_wkt = NULL, has_geometry = FALSE)
  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) meta_nz_cpi,
    eolas_sync_bulk = function(n, path, ...) {
      # Write the parquet to the requested path (simulating a download).
      # src_parquet != path because src is in td, dest is in td2.
      file.copy(src_parquet, path, overwrite = TRUE)
      list(status = "downloaded", path = path, bytes_downloaded = 100L)
    },
    .env = ns
  )

  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    # "nz_cpi" has NO manifest in td (only "doc_huts" does), so it goes
    # through the bulk path → reads the file we wrote above → 3 rows.
    df <- eolas_get_local("nz_cpi", cache_dir = td2, progress = FALSE)
    expect_equal(nrow(df), 3L)
  })
})


# ===========================================================================
# eolas_get() — mode routing interactions with sync library
# ===========================================================================

test_that("eolas_get mode='live' bypasses sync library", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  .setup_sync_library(td, "doc_huts", rows = 99L)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform   = function(req) {
      .fake_resp_lib(DATA_BODY_LIB)
    },
    .env = ns
  )

  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    df <- eolas_get("doc_huts", mode = "live")
    # Live API mock returns 2 rows, not the 99-row library file
    expect_equal(nrow(df), 2L)
  })
})

test_that("eolas_get: EOLAS_LIBRARY without manifest uses normal path", {
  # A library dir that exists but has no manifest for this dataset
  # should fall through to the normal bulk/live path.
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  # Do NOT set up a manifest for "nz_cpi" in td.

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform   = function(req) {
      .fake_resp_lib(DATA_BODY_LIB)
    },
    .env = ns
  )

  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    df <- eolas_get("nz_cpi", mode = "live")
    expect_equal(nrow(df), 2L)
  })
})


# ===========================================================================
# Multi-file library read
# ===========================================================================

test_that("eolas_get_local: multi-file sync library (snapshot + delta) merged correctly", {
  skip_if_not_installed("arrow")
  td   <- withr::local_tempdir()
  ddir <- file.path(td, "doc_huts")
  dir.create(ddir, recursive = TRUE)

  f1 <- file.path(ddir, "snapshot-2026-05-01.parquet")
  f2 <- file.path(ddir, "delta-2026-05-01-to-2026-05-08.parquet")
  arrow::write_parquet(data.frame(x = 1:5, y = "a"), f1)
  arrow::write_parquet(data.frame(x = 6:8, y = "b"), f2)

  manifest <- list(
    dataset   = "doc_huts",
    snapshots = list(
      list(snapshot_id = 1001, kind = "snapshot",
           file = "snapshot-2026-05-01.parquet",
           synced_at = "2026-05-01T10:00:00Z", rows = 5L),
      list(snapshot_id = 1002, kind = "delta",
           parent_snapshot = 1001,
           file = "delta-2026-05-01-to-2026-05-08.parquet",
           synced_at = "2026-05-08T10:00:00Z", rows_added = 3L)
    ),
    current_snapshot = 1002,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  withr::with_envvar(list(EOLAS_LIBRARY = td), {
    df <- eolas_get_local("doc_huts", progress = FALSE)
    expect_equal(nrow(df), 8L)
  })
})
