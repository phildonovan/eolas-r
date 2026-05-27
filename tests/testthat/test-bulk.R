library(testthat)
library(httr2)

# Fake binary content — stands in for a real Parquet file in tests.
# Build as a raw vector without embedding NUL chars in R source.
FAKE_PARQUET <- c(charToRaw("PAR1"), as.raw(rep(0L, 12)), charToRaw("PAR1"))

# Dataset metadata the client fetches first (name -> namespace + table).
BULK_DATASET_META <- jsonlite::toJSON(
  list(
    name      = "nz_cpi",
    title     = "NZ Consumer Price Index",
    source    = "Stats NZ",
    namespace = "statsnz",
    table     = "nz_cpi"
  ),
  auto_unbox = TRUE
)

# ---------------------------------------------------------------------------
# Helper: mock a two-step interaction (metadata lookup + bulk fetch).
# The mock for eolas_http_perform always returns the same response, so we
# encode a stateful counter trick: call 1 = metadata JSON, call 2 = binary.
# ---------------------------------------------------------------------------

with_mock_bulk <- function(bulk_body,
                           bulk_status     = 200L,
                           bulk_content    = "application/octet-stream",
                           meta_body       = BULK_DATASET_META,
                           meta_status     = 200L,
                           code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  call_count <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # First call: GET /v1/datasets/{name}  -> metadata JSON
        structure(
          list(
            method      = "GET",
            url         = "https://api.eolas.fyi/test",
            status_code = meta_status,
            headers     = structure(
              list(`content-type` = "application/json"),
              class = "httr2_headers"
            ),
            body  = charToRaw(meta_body),
            cache = new.env(parent = emptyenv())
          ),
          class = "httr2_response"
        )
      } else {
        # Second call: GET /v1/bulk/...  -> bulk binary
        structure(
          list(
            method      = "GET",
            url         = "https://api.eolas.fyi/test",
            status_code = bulk_status,
            headers     = structure(
              list(`content-type` = bulk_content),
              class = "httr2_headers"
            ),
            body  = if (is.character(bulk_body)) charToRaw(bulk_body)
                    else bulk_body,
            cache = new.env(parent = emptyenv())
          ),
          class = "httr2_response"
        )
      }
    },
    .env = ns
  )
  code
}

# ---------------------------------------------------------------------------
# Happy path
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
    expect_equal(result$value, dest)
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

# ---------------------------------------------------------------------------
# Freshness and format arguments
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# Argument validation
# ---------------------------------------------------------------------------

test_that("invalid format raises an error via match.arg", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_error(
      eolas_download_bulk("nz_cpi", format = "xlsx"),
      "arg"  # match.arg error message contains "arg"
    )
  })
})

test_that("invalid freshness raises an error via match.arg", {
  with_mock_bulk(FAKE_PARQUET, code = {
    expect_error(
      eolas_download_bulk("nz_cpi", freshness = "latest"),
      "arg"
    )
  })
})

# ---------------------------------------------------------------------------
# HTTP error refusal codes
# ---------------------------------------------------------------------------

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
  # Simulate a 404 on the *first* call (metadata lookup).
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"Not found."}', status = 404L)
    },
    .env = ns
  )
  expect_error(eolas_download_bulk("no_such_dataset"), "Not found")
})


# ===========================================================================
# eolas_sync_bulk() tests
# ===========================================================================

# Snapshot id constants used across sync tests.
SNAPSHOT_V1 <- "5503437996448954328"
SNAPSHOT_V2 <- "7041234567890123456"

FAKE_PARQUET_V2 <- c(charToRaw("PAR1"), as.raw(rep(1L, 12)), charToRaw("PAR1"))

# Helper: build a mock HEAD response carrying X-Snapshot-Version.
httr2_mock_head_resp <- function(snapshot_id, status = 200L) {
  structure(
    list(
      method      = "HEAD",
      url         = "https://api.eolas.fyi/test",
      status_code = status,
      headers     = structure(
        list(
          `content-type`       = "application/json",
          `X-Snapshot-Version` = snapshot_id
        ),
        class = "httr2_headers"
      ),
      body  = raw(0L),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# with_mock_sync: three-call sequence (metadata GET, HEAD, optional bulk GET).
# Follows the same pattern as with_mock_bulk() above: .env = ns + code as last arg.
with_mock_sync <- function(snapshot_id,
                           bulk_body    = FAKE_PARQUET,
                           bulk_status  = 200L,
                           bulk_content = "application/octet-stream",
                           meta_body    = BULK_DATASET_META,
                           meta_status  = 200L,
                           code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  call_count <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # Metadata lookup.
        structure(
          list(
            method      = "GET",
            url         = "https://api.eolas.fyi/test",
            status_code = meta_status,
            headers     = structure(list(`content-type` = "application/json"),
                                    class = "httr2_headers"),
            body  = charToRaw(meta_body),
            cache = new.env(parent = emptyenv())
          ),
          class = "httr2_response"
        )
      } else if (call_count == 2L) {
        # HEAD for snapshot version.
        httr2_mock_head_resp(snapshot_id)
      } else {
        # GET for bulk data.
        structure(
          list(
            method      = "GET",
            url         = "https://api.eolas.fyi/test",
            status_code = bulk_status,
            headers     = structure(list(`content-type` = bulk_content),
                                    class = "httr2_headers"),
            body  = if (is.character(bulk_body)) charToRaw(bulk_body) else bulk_body,
            cache = new.env(parent = emptyenv())
          ),
          class = "httr2_response"
        )
      }
    },
    .env = ns
  )
  code
}

# with_mock_sync_unchanged: only two calls (metadata GET + HEAD returning same snapshot).
with_mock_sync_unchanged <- function(snapshot_id,
                                     meta_body = BULK_DATASET_META,
                                     code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  call_count <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        structure(
          list(
            method      = "GET",
            url         = "https://api.eolas.fyi/test",
            status_code = 200L,
            headers     = structure(list(`content-type` = "application/json"),
                                    class = "httr2_headers"),
            body  = charToRaw(meta_body),
            cache = new.env(parent = emptyenv())
          ),
          class = "httr2_response"
        )
      } else {
        httr2_mock_head_resp(snapshot_id)
      }
    },
    .env = ns
  )
  code
}

# ---- helper: write a sidecar next to a file ---------------------------------
write_test_sidecar <- function(data_path, snapshot_id) {
  sidecar_path <- paste0(data_path, ".eolas-meta.json")
  data <- list(
    schema_version = 1L,
    name           = "nz_cpi",
    snapshot_id    = snapshot_id,
    format         = "parquet",
    freshness      = "auto",
    downloaded_at  = "2026-05-24T01:23:45Z",
    source_url     = "https://api.eolas.fyi/v1/bulk/statsnz/nz_cpi?format=parquet"
  )
  writeLines(jsonlite::toJSON(data, auto_unbox = TRUE), sidecar_path)
}

# ---------------------------------------------------------------------------
# First download: no sidecar
# ---------------------------------------------------------------------------

test_that("eolas_sync_bulk first download: status=downloaded, file+sidecar written", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")

  with_mock_sync(SNAPSHOT_V1, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
  })

  expect_equal(result$status, "downloaded")
  expect_true(is.na(result$previous_snapshot_id))
  expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
  expect_equal(result$path, normalizePath(dest, mustWork = FALSE))
  expect_gt(result$bytes_downloaded, 0L)

  expect_true(file.exists(dest))
  expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET)), FAKE_PARQUET)

  sidecar_path <- paste0(normalizePath(dest, mustWork = FALSE), ".eolas-meta.json")
  expect_true(file.exists(sidecar_path))
  meta <- jsonlite::fromJSON(readLines(sidecar_path, warn = FALSE))
  expect_equal(meta$snapshot_id, SNAPSHOT_V1)
})

# ---------------------------------------------------------------------------
# Unchanged: sidecar matches server snapshot
# ---------------------------------------------------------------------------

test_that("eolas_sync_bulk unchanged: no file write, status=unchanged, bytes_downloaded=0", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync_unchanged(SNAPSHOT_V1, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
  })

  expect_equal(result$status, "unchanged")
  expect_equal(result$previous_snapshot_id, SNAPSHOT_V1)
  expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
  expect_equal(result$bytes_downloaded, 0L)
  expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET)), FAKE_PARQUET)
})

# ---------------------------------------------------------------------------
# Updated: server returns new snapshot
# ---------------------------------------------------------------------------

test_that("eolas_sync_bulk updated: file replaced, sidecar updated, status=updated", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync(SNAPSHOT_V2, bulk_body = FAKE_PARQUET_V2, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
  })

  expect_equal(result$status, "updated")
  expect_equal(result$previous_snapshot_id, SNAPSHOT_V1)
  expect_equal(result$current_snapshot_id, SNAPSHOT_V2)
  expect_gt(result$bytes_downloaded, 0L)
  expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET_V2)), FAKE_PARQUET_V2)

  sidecar_path <- paste0(normalizePath(dest, mustWork = FALSE), ".eolas-meta.json")
  meta <- jsonlite::fromJSON(readLines(sidecar_path, warn = FALSE))
  expect_equal(meta$snapshot_id, SNAPSHOT_V2)
})

# ---------------------------------------------------------------------------
# Atomic rename: after a full download completes the destination has the new
# content regardless of whether file.rename or copy-fallback was used.
# (We verify the invariant rather than crashing the rename itself, because
# R's local_mocked_bindings cannot replace base:: functions.)
# ---------------------------------------------------------------------------

test_that("eolas_sync_bulk atomic: destination has new content after update", {
  tmp  <- withr::local_tempdir()
  dest <- file.path(tmp, "nz_cpi.parquet")
  writeBin(FAKE_PARQUET, dest)
  write_test_sidecar(dest, SNAPSHOT_V1)

  with_mock_sync(SNAPSHOT_V2, bulk_body = FAKE_PARQUET_V2, code = {
    result <- eolas_sync_bulk("nz_cpi", path = dest)
  })

  # Destination must have the new content (no partial bytes).
  expect_equal(readBin(dest, "raw", n = length(FAKE_PARQUET_V2)), FAKE_PARQUET_V2)
  expect_equal(result$status, "updated")
  # No orphaned tmp files should remain.
  tmp_files <- list.files(tmp, pattern = "\\.eolas-tmp-", full.names = TRUE)
  expect_length(tmp_files, 0L)
})

# ---------------------------------------------------------------------------
# .eolas_arrow_wkb_to_sf: handles GeoParquet with mixed full + empty WKB rows.
# Regression guard against sfarrow's "vapply(x, is.raw, TRUE) are not all TRUE"
# failure that surfaced on LINZ nz_parcels (21% null geometry rows) — see
# project_geoparquet_evolution.md memo.
# ---------------------------------------------------------------------------

test_that(".eolas_arrow_wkb_to_sf handles empty WKB rows without aborting", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")

  tmp <- withr::local_tempfile(fileext = ".geo.parquet")

  # Build a tiny GeoParquet: 4 rows — 2 real Points, 2 empty geometries.
  # Use sf to construct + sfarrow to write so the GeoParquet metadata is
  # spec-compliant; the empties are introduced via zero-length raw vectors
  # to mirror what arrives from upstream NULL geometries in our pipeline.
  pts <- sf::st_sfc(
    sf::st_point(c(174.7, -36.8)),
    sf::st_point(c(168.4, -44.8)),
    crs = 4326
  )
  wkb_real <- sf::st_as_binary(pts, EWKB = FALSE)  # list of 2 raw vectors

  # Compose the column: real, empty, real, empty.
  wkb_col <- list(wkb_real[[1]], raw(0), wkb_real[[2]], raw(0))

  # Write via arrow with a binary geometry column + minimal GeoParquet
  # metadata block (just enough that read_parquet treats it as binary).
  tbl <- arrow::arrow_table(
    name     = c("a", "b", "c", "d"),
    geometry = arrow::Array$create(wkb_col, type = arrow::binary())
  )
  arrow::write_parquet(tbl, tmp)

  result <- .eolas_arrow_wkb_to_sf(tmp)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 4L)

  empty_rows <- sf::st_is_empty(result$geometry)
  expect_equal(empty_rows, c(FALSE, TRUE, FALSE, TRUE))

  # Decoded points round-trip correctly.
  decoded <- sf::st_coordinates(result$geometry[!empty_rows])
  expect_equal(unname(decoded[1, ]), c(174.7, -36.8))
  expect_equal(unname(decoded[2, ]), c(168.4, -44.8))

  # Other attribute columns are preserved.
  expect_equal(result$name, c("a", "b", "c", "d"))
})

test_that(".eolas_arrow_wkb_to_sf errors clearly on missing geometry column", {
  skip_if_not_installed("arrow")
  tmp <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(arrow::arrow_table(x = 1:3), tmp)

  expect_error(
    .eolas_arrow_wkb_to_sf(tmp),
    "no 'geometry' column"
  )
})
