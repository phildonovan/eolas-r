library(testthat)
library(withr)

# Tests for the as_arrow parameter across eolas_get(), eolas_get_local(),
# and source-specific helpers.

skip_if_not_installed("arrow")

# ---------------------------------------------------------------------------
# Shared constants
# ---------------------------------------------------------------------------

SNAPSHOT_ID <- "snap_abc123"

DATASET_META_NON_GEO <- jsonlite::toJSON(
  list(name = "nz_cpi", title = "NZ CPI", source = "Stats NZ",
       namespace = "statsnz", table = "nz_cpi"),
  auto_unbox = TRUE
)

DATASET_META_GEO <- jsonlite::toJSON(
  list(name = "nz_parcels", title = "NZ Parcels", source = "LINZ",
       namespace = "linz", table = "nz_parcels",
       geometry_type = "MultiPolygon",
       bulk_export_class = "geoparquet",
       row_count_at_last_refresh = 3000000L),
  auto_unbox = TRUE
)

# Helper: write a minimal parquet file using arrow
write_test_parquet <- function(path, df) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path)
}

# Fake sync result list
fake_sync_result <- function(path) {
  list(
    status               = "downloaded",
    previous_snapshot_id = NA_character_,
    current_snapshot_id  = SNAPSHOT_ID,
    path                 = normalizePath(path, mustWork = FALSE),
    bytes_downloaded     = 1024L
  )
}

# ---------------------------------------------------------------------------
# eolas_get_local(): as_arrow=TRUE on a non-geo parquet returns arrow::Table
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns arrow::Table for non-geo dataset (as_arrow=TRUE)", {
  tmp <- withr::local_tempdir()
  parquet_path <- file.path(tmp, "nz_cpi.parquet")
  df_orig <- data.frame(date = "2023-01-01", value = 1100.5, stringsAsFactors = FALSE)
  write_test_parquet(parquet_path, df_orig)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      fake_sync_result(path)
    },
    .env = ns
  )

  result <- eolas_get_local("nz_cpi", cache_dir = tmp, as_arrow = TRUE)

  expect_true(inherits(result, "ArrowTabular"))
  expect_true("value" %in% names(result))
})

# ---------------------------------------------------------------------------
# eolas_get_local(): as_arrow=TRUE on a geo geoparquet returns arrow::Table
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns arrow::Table for geo dataset (as_arrow=TRUE)", {
  tmp <- withr::local_tempdir()
  geo_parquet_path <- file.path(tmp, "nz_parcels.geo.parquet")
  df_geo <- data.frame(
    id           = 1L,
    geometry_wkt = "POINT (174.76 -36.85)",
    stringsAsFactors = FALSE
  )
  write_test_parquet(geo_parquet_path, df_geo)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      fake_sync_result(path)
    },
    .env = ns
  )

  result <- eolas_get_local("nz_parcels", cache_dir = tmp, as_arrow = TRUE)

  expect_true(inherits(result, "ArrowTabular"))
  # geometry_wkt stays as a character column — no sf conversion
  expect_true("geometry_wkt" %in% names(result))
})

# ---------------------------------------------------------------------------
# eolas_get_local(): default unchanged (regression test)
# ---------------------------------------------------------------------------

test_that("eolas_get_local default still returns data.frame (regression)", {
  tmp <- withr::local_tempdir()
  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-01-01,1100.5"), con)
  close(con)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      fake_sync_result(path)
    },
    .env = ns
  )

  result <- eolas_get_local("nz_cpi", cache_dir = tmp, format = "csv_gz")

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "ArrowTabular"))
})

# ---------------------------------------------------------------------------
# Conflict: as_arrow=TRUE + as_sf=TRUE raises stop()
# ---------------------------------------------------------------------------

test_that("eolas_get_local stops with clear error when as_arrow=TRUE and as_sf=TRUE", {
  expect_error(
    eolas_get_local("nz_parcels", as_arrow = TRUE, as_sf = TRUE),
    "mutually exclusive",
    fixed = TRUE
  )
})

test_that("eolas_get stops with clear error when as_arrow=TRUE and as_sf=TRUE", {
  expect_error(
    eolas_get("nz_parcels", as_arrow = TRUE, as_sf = TRUE),
    "mutually exclusive",
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# eolas_get(): as_arrow on the live path
# ---------------------------------------------------------------------------

test_that("eolas_get live path with as_arrow=TRUE returns arrow::Table", {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    .eolas_fetch_df = function(name, params, base_url) {
      data.frame(date = "2023-01-01", value = 1100.5, stringsAsFactors = FALSE)
    },
    .env = ns
  )

  result <- eolas_get("nz_cpi", as_arrow = TRUE)

  expect_true(inherits(result, "ArrowTabular"))
  expect_true("value" %in% names(result))
})

# ---------------------------------------------------------------------------
# Source helper: eolas_get_linz() with as_arrow=TRUE (live path)
# ---------------------------------------------------------------------------

test_that("eolas_get_linz with as_arrow=TRUE returns arrow::Table", {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    .eolas_fetch_df = function(name, params, base_url) {
      data.frame(id = 1L, geometry_wkt = "POINT (174.76 -36.85)",
                 stringsAsFactors = FALSE)
    },
    .env = ns
  )

  result <- eolas_get_linz("nz_parcels", as_arrow = TRUE)

  expect_true(inherits(result, "ArrowTabular"))
})

# ---------------------------------------------------------------------------
# as_arrow=TRUE on csv_gz returns arrow::Table
# ---------------------------------------------------------------------------

test_that("eolas_get_local as_arrow=TRUE with csv_gz format returns arrow::Table", {
  tmp <- withr::local_tempdir()
  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-01-01,1100.5"), con)
  close(con)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      fake_sync_result(path)
    },
    .env = ns
  )

  result <- eolas_get_local("nz_cpi",
                             cache_dir = tmp,
                             format    = "csv_gz",
                             as_arrow  = TRUE)

  expect_true(inherits(result, "ArrowTabular"))
  expect_true("value" %in% names(result))
})
