library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

httr2_mock_resp <- function(body, status = 200L) {
  structure(
    list(
      method = "GET",
      url = "https://api.eolas.fyi/test",
      status_code = status,
      headers = structure(list(`content-type` = "application/json"), class = "httr2_headers"),
      body = charToRaw(body),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

with_mock_eolas <- function(body, status = 200L, code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)
  local_mocked_bindings(
    eolas_http_perform = function(...) httr2_mock_resp(body, status),
    .env = ns
  )
  code
}

DATASET_LIST_BODY <- '[
  {"name":"nz_cpi","title":"NZ CPI","source":"Stats NZ","namespace":"statsnz","description":""},
  {"name":"nz_gdp","title":"NZ GDP","source":"OECD","namespace":"oecd","description":""}
]'

DATA_BODY <- '{"data":[
  {"date":"2023-01-01","period":"2023Q1","value":100.0},
  {"date":"2023-04-01","period":"2023Q2","value":101.5}
]}'

# ---------------------------------------------------------------------------
# eolas_key / auth
# ---------------------------------------------------------------------------

test_that("eolas_key returns key invisibly", {
  result <- withVisible(eolas_key("eolas_testkey"))
  expect_false(result$visible)
  expect_equal(result$value, "eolas_testkey")
})

test_that("eolas_get_key_internal reads EOLAS_API_KEY env var when no session key", {
  ns <- getNamespace("eolas")
  assign("key", NULL, envir = ns$.eolas_env)
  withr::with_envvar(c(EOLAS_API_KEY = "eolas_from_env"), {
    expect_equal(ns$eolas_get_key_internal(), "eolas_from_env")
  })
})

test_that("eolas_get_key_internal errors when no key set anywhere", {
  ns <- getNamespace("eolas")
  assign("key", NULL, envir = ns$.eolas_env)
  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    expect_error(ns$eolas_get_key_internal(), "No API key found")
  })
})

# ---------------------------------------------------------------------------
# eolas_list
# ---------------------------------------------------------------------------

test_that("eolas_list returns a data frame", {
  with_mock_eolas(DATASET_LIST_BODY, code = {
    result <- eolas_list()
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2L)
  })
})

test_that("eolas_list filters by source", {
  with_mock_eolas(DATASET_LIST_BODY, code = {
    result <- eolas_list("Stats NZ")
    expect_equal(nrow(result), 1L)
    expect_equal(result$name, "nz_cpi")
  })
})

# ---------------------------------------------------------------------------
# vs_list_* convenience wrappers
# ---------------------------------------------------------------------------

test_that("eolas_list_statsnz returns only Stats NZ rows", {
  with_mock_eolas(DATASET_LIST_BODY, code = {
    result <- eolas_list_statsnz()
    expect_equal(nrow(result), 1L)
    expect_equal(result$source, "Stats NZ")
  })
})

test_that("eolas_list_oecd returns only OECD rows", {
  with_mock_eolas(DATASET_LIST_BODY, code = {
    result <- eolas_list_oecd()
    expect_equal(nrow(result), 1L)
    expect_equal(result$source, "OECD")
  })
})

# ---------------------------------------------------------------------------
# eolas_info
# ---------------------------------------------------------------------------

test_that("eolas_info returns a list", {
  with_mock_eolas('{"name":"nz_cpi","title":"NZ Consumer Price Index","source":"Stats NZ"}', code = {
    result <- eolas_info("nz_cpi")
    expect_type(result, "list")
    expect_equal(result$name, "nz_cpi")
  })
})

# ---------------------------------------------------------------------------
# eolas_get
# ---------------------------------------------------------------------------

test_that("eolas_get returns a eolas_dataset with Date column", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get("nz_cpi")
    expect_s3_class(df, "eolas_dataset")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 2L)
    expect_s3_class(df$date, "Date")
    expect_equal(attr(df, "eolas_name"), "nz_cpi")
  })
})

# ---------------------------------------------------------------------------
# vs_get_* source functions
# ---------------------------------------------------------------------------

test_that("eolas_get_statsnz tags result with Stats NZ source", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get_statsnz("nz_cpi")
    expect_s3_class(df, "eolas_dataset")
    expect_equal(attr(df, "eolas_source"), "Stats NZ")
  })
})

test_that("eolas_get_oecd tags result with OECD source", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get_oecd("nz_gdp")
    expect_equal(attr(df, "eolas_source"), "OECD")
  })
})

test_that("eolas_get_treasury tags result with NZ Treasury source", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get_treasury("treasury_fiscal_spending")
    expect_equal(attr(df, "eolas_source"), "NZ Treasury")
  })
})

# ---------------------------------------------------------------------------
# print.eolas_dataset
# ---------------------------------------------------------------------------

test_that("print.eolas_dataset includes series name and row count", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get_statsnz("nz_cpi")
    output <- capture.output(print(df))
    expect_true(any(grepl("nz_cpi", output)))
    expect_true(any(grepl("Stats NZ", output)))
    expect_true(any(grepl("2 rows", output)))
  })
})

# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("401 raises authentication error", {
  with_mock_eolas('{"detail":"Unauthorised"}', status = 401L, code = {
    expect_error(eolas_get("nz_cpi"), "Authentication error")
  })
})

test_that("429 raises rate limit error", {
  with_mock_eolas('{"detail":"Daily limit"}', status = 429L, code = {
    expect_error(eolas_get("nz_cpi"), "Rate limit")
  })
})

test_that("404 raises not found error", {
  with_mock_eolas('{"detail":"Series not found"}', status = 404L, code = {
    expect_error(eolas_get("bad_series"), "Not found")
  })
})

# ---------------------------------------------------------------------------
# Geospatial — as_sf
# ---------------------------------------------------------------------------

GEO_DATA_BODY <- '{"data":[
  {"address_id":1,"full_address":"1 Main Rd","geometry_wkt":"POINT (174.78 -41.28)"},
  {"address_id":2,"full_address":"2 Main Rd","geometry_wkt":"POINT (174.79 -41.29)"}
]}'

test_that("eolas_get auto-converts to sf when geometry_wkt + sf available", {
  skip_if_not_installed("sf")
  with_mock_eolas(GEO_DATA_BODY, code = {
    df <- eolas_get("nz_addresses")
    expect_s3_class(df, "sf")
    expect_true("geometry" %in% names(df))
    expect_false("geometry_wkt" %in% names(df))
    expect_equal(sf::st_crs(df)$epsg, 4326)
  })
})

test_that("eolas_get with as_sf=FALSE keeps geometry_wkt as a string column", {
  with_mock_eolas(GEO_DATA_BODY, code = {
    df <- eolas_get("nz_addresses", as_sf = FALSE)
    expect_false(inherits(df, "sf"))
    expect_true("geometry_wkt" %in% names(df))
    expect_true(is.character(df$geometry_wkt))
  })
})

test_that("eolas_get on non-geospatial dataset returns eolas_dataset even when sf is installed", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get("nz_cpi")
    expect_false(inherits(df, "sf"))
    expect_s3_class(df, "eolas_dataset")
  })
})
