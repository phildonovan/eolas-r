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
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"Not found."}', status = 404L)
    },
    .env = ns
  )
  expect_error(eolas_download_bulk("no_such_dataset"), "Not found")
})
