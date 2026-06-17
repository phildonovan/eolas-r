library(testthat)
library(withr)

# Tests for the as_arrow parameter across eolas_get(), eolas_get_local(),
# and source-specific helpers.

skip_if_not_installed("arrow")

write_test_parquet <- function(path, df) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path)
}

test_that("eolas_get_local returns arrow::Table for non-geo dataset (as_arrow=TRUE)", {
  tmp <- withr::local_tempdir()
  parquet_path <- file.path(tmp, "nz_cpi.parquet")
  df_orig <- data.frame(date = "2023-01-01", value = 1100.5, stringsAsFactors = FALSE)
  write_test_parquet(parquet_path, df_orig)

  with_mock_get_local(DATASET_META_NON_GEO, function(path) invisible(NULL), {
    result <- eolas_get_local("nz_cpi", cache_dir = tmp, as_arrow = TRUE)
    expect_true(inherits(result, "ArrowTabular"))
    expect_true("value" %in% names(result))
  })
})

test_that("eolas_get_local returns arrow::Table for geo dataset (as_arrow=TRUE)", {
  tmp <- withr::local_tempdir()
  geo_parquet_path <- file.path(tmp, "nz_parcels.geo.parquet")
  df_geo <- data.frame(
    id           = 1L,
    geometry_wkt = "POINT (174.76 -36.85)",
    stringsAsFactors = FALSE
  )
  write_test_parquet(geo_parquet_path, df_geo)

  with_mock_get_local(DATASET_META_GEO, function(path) invisible(NULL), {
    result <- eolas_get_local("nz_parcels", cache_dir = tmp, as_arrow = TRUE)
    expect_true(inherits(result, "ArrowTabular"))
    expect_true("geometry_wkt" %in% names(result))
  })
})

test_that("eolas_get_local default still returns data.frame (regression)", {
  tmp <- withr::local_tempdir()
  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-01-01,1100.5"), con)
  close(con)

  with_mock_get_local(DATASET_META_NON_GEO, function(path) invisible(NULL), {
    result <- eolas_get_local("nz_cpi", cache_dir = tmp, format = "csv_gz")
    expect_s3_class(result, "data.frame")
    expect_false(inherits(result, "ArrowTabular"))
  })
})

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

test_that("eolas_get live path with as_arrow=TRUE returns arrow::Table", {
  set_test_key()
  with_mocked_bindings(
    {
      result <- eolas_get("nz_cpi", as_arrow = TRUE)
      expect_true(inherits(result, "ArrowTabular"))
      expect_true("value" %in% names(result))
    },
    .eolas_fetch_df = function(name, params, base_url) {
      data.frame(date = "2023-01-01", value = 1100.5, stringsAsFactors = FALSE)
    },
    .package = "eolas"
  )
})

test_that("eolas_get_linz with as_arrow=TRUE returns arrow::Table", {
  set_test_key()
  with_mocked_bindings(
    {
      result <- eolas_get_linz("nz_parcels", as_arrow = TRUE)
      expect_true(inherits(result, "ArrowTabular"))
    },
    .eolas_fetch_df = function(name, params, base_url) {
      data.frame(id = 1L, geometry_wkt = "POINT (174.76 -36.85)",
                 stringsAsFactors = FALSE)
    },
    .package = "eolas"
  )
})

test_that("eolas_get_local as_arrow=TRUE with csv_gz format returns arrow::Table", {
  tmp <- withr::local_tempdir()
  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-01-01,1100.5"), con)
  close(con)

  with_mock_get_local(DATASET_META_NON_GEO, function(path) invisible(NULL), {
    result <- eolas_get_local("nz_cpi",
                               cache_dir = tmp,
                               format    = "csv_gz",
                               as_arrow  = TRUE)
    expect_true(inherits(result, "ArrowTabular"))
    expect_true("value" %in% names(result))
  })
})