library(testthat)

# Helpers (FAKE_PARQUET, with_mock_bulk, with_mock_sync, ...) live in helper.R.

# ---------------------------------------------------------------------------
# Happy path — eolas_download_bulk
# ---------------------------------------------------------------------------

test_that("eolas_download_bulk returns raw bytes when path = NULL", {
  with_mock_bulk(FAKE_PARQUET, code = {
    result <- eolas_download_bulk("nz_cpi")
    expect_type(result, "raw")
    expect_equal(result, FAKE_PARQUET)
  })
})

test_that("eolas_download_bulk writes file and returns path invisibly when path is set", {
  tmp <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  with_mock_bulk(FAKE_PARQUET, code = {
    result <- withVisible(eolas_download_bulk("nz_cpi", path = dest))
    expect_false(result$visible)
    expect_same_path(result$value, dest)
    expect_true(file.exists(dest))
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET)), FAKE_PARQUET)
  })
})

test_that("eolas_download_bulk creates parent directories automatically", {
  tmp <- withr::local_tempdir()
  dest <- file.path(tmp, "nested", "dir", "nz_cpi.parquet")
  with_mock_bulk(FAKE_PARQUET, code = {
    eolas_download_bulk("nz_cpi", path = dest)
    expect_true(file.exists(dest))
  })
})

test_that("freshness = 'auto' is accepted without error", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_no_error(eolas_download_bulk("nz_cpi", freshness = "auto"))
  })
})

test_that("freshness = 'monthly' is accepted", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_no_error(eolas_download_bulk("nz_cpi", freshness = "monthly"))
  })
})

test_that("freshness = 'current' is accepted", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_no_error(eolas_download_bulk("nz_cpi", freshness = "current"))
  })
})

test_that("format = 'csv_gz' is accepted", {
  with_mock_bulk(charToRaw("csv data"), code = {
    result <- eolas_download_bulk("nz_cpi", format = "csv_gz")
    expect_type(result, "raw")
  })
})

test_that("format = 'geoparquet' is accepted", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_no_error(eolas_download_bulk("nz_cpi", format = "geoparquet"))
  })
})

test_that("invalid format raises an error via match.arg", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_error(eolas_download_bulk("nz_cpi", format = "xlsx"), "arg")
  })
})

test_that("invalid freshness raises an error via match.arg", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_error(eolas_download_bulk("nz_cpi", freshness = "latest"), "arg")
  })
})

test_that("HTTP 402 raises bulk upgrade required error", {
  body_402 <- '{"detail":"Fresh bulk downloads are a Pro feature. Free accounts get the latest monthly snapshot."}'
  with_mock_bulk(body_402, bulk_status = 402L, bulk_content = "application/json", code = {
    expect_error(
      eolas_download_bulk("nz_cpi", freshness = "current"),
      "Bulk upgrade required",
      fixed = TRUE
    )
  })
})

test_that("HTTP 403 with licence detail raises bulk licence restricted error", {
  body_403 <- '{"detail":"This dataset is not available as a bulk download (licence: OECD)."}'
  oecd_meta <- jsonlite::toJSON(
    list(name = "oecd_gdp", namespace = "oecd", table = "oecd_gdp"),
    auto_unbox = TRUE
  )
  with_mock_bulk(body_403,
                 bulk_status  = 403L,
                 bulk_content = "application/json",
                 meta_body    = oecd_meta,
                 code = {
    expect_error(
      eolas_download_bulk("oecd_gdp"),
      "Bulk licence restricted",
      fixed = TRUE
    )
  })
})

test_that("HTTP 503 raises bulk not yet available error", {
  body_503 <- '{"detail":"Monthly bulk snapshots are still rolling out."}'
  with_mock_bulk(body_503, bulk_status = 503L, bulk_content = "application/json", code = {
    expect_error(
      eolas_download_bulk("nz_cpi"),
      "Bulk not yet available",
      fixed = TRUE
    )
  })
})

test_that("HTTP 404 on metadata lookup raises not found error", {
  set_test_key()
  with_mocked_bindings(
    {
      expect_error(eolas_download_bulk("no_such_dataset"), "Not found")
    },
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"Not found."}', status = 404L)
    },
    .package = "eolas"
  )
})

# ---------------------------------------------------------------------------
# eolas_sync_bulk
# ---------------------------------------------------------------------------

test_that("eolas_sync_bulk first download: status=downloaded, file+sidecar written", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")

  with_mock_sync(SNAPSHOT_V1, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
    expect_equal(result$status, "downloaded")
    expect_true(is.na(result$previous_snapshot_id))
    expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
    expect_same_path(result$path, dest)
    expect_gt(result$bytes_downloaded, 0L)
    expect_true(file.exists(dest))
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET)), FAKE_PARQUET)
    sidecar_path <- paste0(normalizePath(dest, winslash = "/", mustWork = TRUE), ".eolas-meta.json")
    expect_true(file.exists(sidecar_path))
    meta <- jsonlite::fromJSON(readLines(sidecar_path, warn = FALSE))
    expect_equal(meta$snapshot_id, SNAPSHOT_V1)
  })
})

test_that("eolas_sync_bulk unchanged: no file write, status=unchanged, bytes_downloaded=0", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync_unchanged(SNAPSHOT_V1, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
    expect_equal(result$status, "unchanged")
    expect_equal(result$previous_snapshot_id, SNAPSHOT_V1)
    expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
    expect_equal(result$bytes_downloaded, 0L)
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET)), FAKE_PARQUET)
  })
})

test_that("eolas_sync_bulk force=TRUE re-downloads when snapshot unchanged", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync(SNAPSHOT_V1, bulk_body = FAKE_PARQUET_V2, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest, force = TRUE)
    expect_equal(result$status, "updated")
    expect_equal(result$previous_snapshot_id, SNAPSHOT_V1)
    expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
    expect_gt(result$bytes_downloaded, 0L)
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET_V2)), FAKE_PARQUET_V2)
  })
})

test_that("eolas_cache_clear removes cached bulk files and sidecars", {
  tmp <- withr::local_tempdir()
  parquet <- file.path(tmp, "nz_parcels.parquet")
  geo     <- file.path(tmp, "nz_parcels.geo.parquet")
  writeBin(charToRaw("p"), parquet)
  writeBin(charToRaw("g"), geo)
  writeLines("{}", paste0(parquet, ".eolas-meta.json"))
  writeLines("{}", paste0(geo, ".eolas-meta.json"))

  cleared <- eolas_cache_clear("nz_parcels", cache_dir = tmp)
  expect_length(cleared$files, 4L)
  expect_false(any(file.exists(c(parquet, geo, paste0(parquet, ".eolas-meta.json"),
                                 paste0(geo, ".eolas-meta.json")))))
})

test_that("eolas_cache_clear with format deletes only that variant", {
  tmp <- withr::local_tempdir()
  parquet <- file.path(tmp, "nz_cpi.parquet")
  csv     <- file.path(tmp, "nz_cpi.csv.gz")
  writeBin(charToRaw("p"), parquet)
  writeBin(charToRaw("c"), csv)

  cleared <- eolas_cache_clear("nz_cpi", cache_dir = tmp, format = "parquet")
  expect_length(cleared$files, 1L)
  expect_false(file.exists(parquet))
  expect_true(file.exists(csv))
})

test_that("eolas_cache_clear can drop session metadata without deleting files", {
  set_test_key()
  key <- eolas:::.eolas_meta_cache_key("nz_cpi", "https://api.eolas.fyi")
  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      eolas:::.eolas_parse_info_response(
        jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
      )
    },
    .package = "eolas"
  )
  eolas:::.eolas_info_cached("nz_cpi", base_url = "https://api.eolas.fyi")
  expect_true(exists(key, envir = eolas:::.eolas_meta_cache, inherits = FALSE))

  cleared <- eolas_cache_clear("nz_cpi", files = FALSE)
  expect_equal(cleared$meta_cleared, 1L)
  expect_length(cleared$files, 0L)
  expect_false(exists(key, envir = eolas:::.eolas_meta_cache, inherits = FALSE))
})

test_that("eolas_get force=TRUE is ignored on live API path", {
  info_calls <- 0L
  set_test_key()
  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      info_calls <<- info_calls + 1L
      eolas:::.eolas_parse_info_response(
        jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
      )
    },
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        httr2_mock_resp('{"data":[{"date":"2023-01-01","value":1}]}', 200L)
      } else {
        stop("unexpected URL", call. = FALSE)
      }
    },
    .package = "eolas"
  )

  # Prime session metadata cache, then force on live path should not clear it.
  eolas_get("nz_cpi")
  info_calls_before <- info_calls
  eolas_get("nz_cpi", force = TRUE)
  expect_equal(info_calls, info_calls_before)
})

test_that("eolas_sync_bulk updated: file replaced, sidecar updated, status=updated", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync(SNAPSHOT_V2, bulk_body = FAKE_PARQUET_V2, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
    expect_equal(result$status, "updated")
    expect_equal(result$previous_snapshot_id, SNAPSHOT_V1)
    expect_equal(result$current_snapshot_id, SNAPSHOT_V2)
    expect_gt(result$bytes_downloaded, 0L)
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET_V2)), FAKE_PARQUET_V2)
    sidecar_path <- paste0(normalizePath(dest, mustWork = FALSE), ".eolas-meta.json")
    meta <- jsonlite::fromJSON(readLines(sidecar_path, warn = FALSE))
    expect_equal(meta$snapshot_id, SNAPSHOT_V2)
  })
})

test_that("eolas_sync_bulk atomic: destination has new content after update", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync(SNAPSHOT_V2, bulk_body = FAKE_PARQUET_V2, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
    expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET_V2)), FAKE_PARQUET_V2)
    expect_equal(result$status, "updated")
    tmp_files <- list.files(tmp, pattern = "\\.eolas-tmp-", full.names = TRUE)
    expect_length(tmp_files, 0L)
  })
})

test_that(".eolas_arrow_wkb_to_sf handles empty WKB rows without aborting", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")

  tmp <- withr::local_tempfile(fileext = ".geo.parquet")
  pts <- sf::st_sfc(
    sf::st_point(c(174.7, -36.8)),
    sf::st_point(c(168.4, -44.8)),
    crs = 4326
  )
  wkb_real <- sf::st_as_binary(pts, EWKB = FALSE)
  wkb_col <- list(wkb_real[[1]], raw(0), wkb_real[[2]], raw(0))
  tbl <- arrow::arrow_table(
    name     = c("a", "b", "c", "d"),
    geometry = arrow::Array$create(wkb_col, type = arrow::binary())
  )
  arrow::write_parquet(tbl, tmp)

  result <- eolas:::.eolas_arrow_wkb_to_sf(tmp)

  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4L)
  empty_rows <- sf::st_is_empty(result$geometry)
  expect_equal(empty_rows, c(FALSE, TRUE, FALSE, TRUE))
  decoded <- sf::st_coordinates(result$geometry[!empty_rows])
  expect_equal(unname(decoded[1, ]), c(174.7, -36.8))
  expect_equal(unname(decoded[2, ]), c(168.4, -44.8))
  expect_equal(result$name, c("a", "b", "c", "d"))
})

test_that(".eolas_arrow_wkb_to_sf errors clearly on missing geometry column", {
  skip_if_not_installed("arrow")
  tmp <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(arrow::arrow_table(x = 1:3), tmp)
  expect_error(eolas:::.eolas_arrow_wkb_to_sf(tmp), "no 'geometry' column")
})