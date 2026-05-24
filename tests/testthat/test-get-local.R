library(testthat)
library(withr)

# Tests for eolas_get_local() -- the notebook-friendly whole-dataset convenience.

# ---------------------------------------------------------------------------
# Shared constants
# ---------------------------------------------------------------------------

FAKE_PARQUET_LOCAL <- c(charToRaw("PAR1"), as.raw(rep(0L, 12)), charToRaw("PAR1"))

DATASET_META_NON_GEO <- jsonlite::toJSON(
  list(name = "nz_cpi", title = "NZ CPI", source = "Stats NZ",
       namespace = "statsnz", table = "nz_cpi"),
  auto_unbox = TRUE
)

DATASET_META_GEO <- jsonlite::toJSON(
  list(name = "nz_parcels", title = "NZ Parcels", source = "LINZ",
       namespace = "linz", table = "nz_parcels",
       geometry_type = "MultiPolygon"),
  auto_unbox = TRUE
)

SNAPSHOT_ID <- "snap_abc123"

# ---------------------------------------------------------------------------
# with_mock_get_local: mock the three underlying calls made by eolas_get_local
# (info, sync_bulk's info call, HEAD, and optional GET).
# Strategy: mock eolas_sync_bulk directly so get_local only needs to read the
# file — decoupling from the full sync_bulk call chain.
# ---------------------------------------------------------------------------

with_mock_get_local <- function(name,
                                meta_json,
                                file_writer,     # function(path) -> writes a file to path
                                code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(meta_json, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      file_writer(path)
      list(
        status               = "downloaded",
        previous_snapshot_id = NA_character_,
        current_snapshot_id  = SNAPSHOT_ID,
        path                 = normalizePath(path, mustWork = FALSE),
        bytes_downloaded     = 1024L
      )
    },
    .env = ns
  )
  code
}

# ---------------------------------------------------------------------------
# First call returns data.frame (non-geo, via arrow or CSV)
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns data.frame for non-geo dataset (first call)", {
  tmp <- withr::local_tempdir()

  # Write a CSV.gz file because arrow may not be available in the test env;
  # test with csv_gz format to stay dependency-free.
  file_writer <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- gzcon(file(path, "wb"))
    writeLines(c("date,value", "2023-01-01,1100.5"), con)
    close(con)
  }

  with_mock_get_local("nz_cpi", DATASET_META_NON_GEO, file_writer, {
    result <- eolas_get_local("nz_cpi",
                              format    = "csv_gz",
                              cache_dir = tmp)
  })

  expect_s3_class(result, "data.frame")
  expect_true("date" %in% names(result))
  expect_true("value" %in% names(result))
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Subsequent call returns data.frame from cached file (sync_bulk = unchanged)
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns data.frame from cached file on subsequent call", {
  tmp <- withr::local_tempdir()

  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-06-01,1105.0"), con)
  close(con)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      # Simulates "unchanged" — does NOT write the file (it already exists).
      list(
        status               = "unchanged",
        previous_snapshot_id = SNAPSHOT_ID,
        current_snapshot_id  = SNAPSHOT_ID,
        path                 = normalizePath(path, mustWork = FALSE),
        bytes_downloaded     = 0L
      )
    },
    .env = ns
  )

  result <- eolas_get_local("nz_cpi", format = "csv_gz", cache_dir = tmp)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Tilde expansion in cache_dir
# ---------------------------------------------------------------------------

test_that("eolas_get_local expands ~ in cache_dir to an absolute path", {
  # Override HOME so we don't write to the real ~/.cache
  withr::local_envvar(HOME = withr::local_tempdir())

  actual_path_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      actual_path_seen <<- path
      # Write a stub CSV.gz so the read succeeds.
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      con <- gzcon(file(path, "wb"))
      writeLines(c("x", "1"), con)
      close(con)
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 100L)
    },
    .env = ns
  )

  eolas_get_local("nz_cpi", format = "csv_gz", cache_dir = "~/.cache/eolas")

  expect_false(grepl("~", actual_path_seen, fixed = TRUE))
  expect_true(startsWith(actual_path_seen, "/"))
})

# ---------------------------------------------------------------------------
# Auto-detect format: geo dataset -> geoparquet format selected
# ---------------------------------------------------------------------------

test_that("eolas_get_local auto-detects geoparquet for geo datasets", {
  tmp <- withr::local_tempdir()

  format_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      # geometry_type present -> geo dataset
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      format_seen <<- format
      # Don't write a real GeoParquet (complex); just record the format used.
      # The read step will fail, but we only test format detection here.
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 1024L)
    },
    .env = ns
  )

  # Suppress the expected read failure — we only care about format selection.
  tryCatch(
    eolas_get_local("nz_parcels", cache_dir = tmp),
    error = function(e) NULL
  )

  expect_equal(format_seen, "geoparquet")
})

# ---------------------------------------------------------------------------
# Auto-detect format: non-geo dataset -> parquet format selected
# ---------------------------------------------------------------------------

test_that("eolas_get_local auto-detects parquet for non-geo datasets", {
  tmp <- withr::local_tempdir()

  format_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      format_seen <<- format
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 1024L)
    },
    .env = ns
  )

  # Arrow likely not available; suppress read error, only test format selection.
  tryCatch(
    eolas_get_local("nz_cpi", cache_dir = tmp),
    error = function(e) NULL
  )

  expect_equal(format_seen, "parquet")
})

# ---------------------------------------------------------------------------
# Bulk error propagation: stop() messages pass through unchanged
# ---------------------------------------------------------------------------

test_that("eolas_get_local propagates Bulk upgrade required stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk upgrade required: Fresh bulk downloads are a Pro feature.", call. = FALSE)
    },
    .env = ns
  )

  expect_error(
    eolas_get_local("nz_cpi", freshness = "current", cache_dir = tmp),
    "Bulk upgrade required",
    fixed = TRUE
  )
})

test_that("eolas_get_local propagates Bulk licence restricted stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(
        jsonlite::toJSON(
          list(name = "oecd_gdp", namespace = "oecd", table = "oecd_gdp",
               source = "OECD"),
          auto_unbox = TRUE
        ),
        simplifyVector = FALSE
      )
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk licence restricted: This dataset is not available (licence: OECD).",
           call. = FALSE)
    },
    .env = ns
  )

  expect_error(
    eolas_get_local("oecd_gdp", cache_dir = tmp),
    "Bulk licence restricted",
    fixed = TRUE
  )
})

test_that("eolas_get_local propagates Bulk not yet available stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk not yet available: Monthly bulk snapshots are still rolling out.",
           call. = FALSE)
    },
    .env = ns
  )

  expect_error(
    eolas_get_local("nz_cpi", cache_dir = tmp),
    "Bulk not yet available",
    fixed = TRUE
  )
})
