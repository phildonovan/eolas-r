library(testthat)

# httr2_mock_resp(), with_mock_eolas(), httr2_mock_resp_empty() live in helper.R.

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
  # Mock the keyring + config-file fallbacks to empty so the test is hermetic
  # regardless of the dev machine's real keyring entry / ~/.eolas/config.json.
  testthat::local_mocked_bindings(
    .keyring_get = function() "",
    .config_file_get_key = function() ""
  )
  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    expect_error(ns$eolas_get_key_internal(), "No API key found")
  })
})

# ---------------------------------------------------------------------------
# eolas_list
# ---------------------------------------------------------------------------

test_that("eolas_list returns a tibble", {
  with_mock_eolas(DATASET_LIST_BODY, code = {
    result <- eolas_list()
    expect_s3_class(result, "tbl_df")
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
# eolas_list_* convenience wrappers
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

test_that("eolas_info returns a one-row tibble", {
  with_mock_eolas('{"name":"nz_cpi","title":"NZ Consumer Price Index","source":"Stats NZ"}', code = {
    result <- eolas_info("nz_cpi")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1L)
    expect_equal(result$name, "nz_cpi")
  })
})

test_that("eolas_info parses live-shaped JSON (null has_geometry, snapshots, column series_id)", {
  live_body <- '{"name":"nz_cpi","namespace":"oecd","source":"OECD","country":"OECD",
    "title":"NZ CPI inflation (annual % change)","description":"Quarterly CPI YoY.",
    "bulk_export_class":"none","geometry_type":"none","has_geometry":null,
    "row_count_at_last_refresh":145,"licence":"OECD Terms",
    "current_snapshot_id":7406634567890123456,"refresh_cadence":"monthly",
    "last_refreshed_at":"2026-06-14T18:02:02+00:00",
    "previous_snapshots":[5567890123456789012,7478901234567890123],
    "columns":[
      {"name":"date","type":"date","description":"Observation date","series_id":null},
      {"name":"value","type":"double","description":"Value","series_id":null}
    ]}'
  set_test_key()
  with_mocked_bindings({
    result <- eolas_info("nz_cpi")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1L)
    expect_equal(result$name, "nz_cpi")
    expect_length(result$previous_snapshots[[1]], 2L)
    expect_true(is.na(result$has_geometry[[1]]))
    cols <- result$columns[[1]]
    expect_equal(nrow(cols), 2L)
    expect_equal(cols$name, c("date", "value"))
    expect_true(all(is.na(cols$series_id)))
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp(live_body),
  .package = "eolas")
})

# ---------------------------------------------------------------------------
# eolas_get
# ---------------------------------------------------------------------------

test_that("eolas_get returns a eolas_dataset tibble with Date column", {
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get("nz_cpi")
    expect_s3_class(df, "eolas_dataset")
    expect_s3_class(df, "tbl_df")
    expect_equal(nrow(df), 2L)
    expect_s3_class(df$date, "Date")
    expect_equal(attr(df, "eolas_name"), "nz_cpi")
  })
})

test_that("eolas_get limit returns most recent dated rows", {
  body <- '{"data":[
    {"date":"2018-01-01","period":"2018Q1","value":1.0},
    {"date":"2020-01-01","period":"2020Q1","value":2.0},
    {"date":"2024-01-01","period":"2024Q1","value":3.0},
    {"date":"2025-01-01","period":"2025Q1","value":4.0}
  ]}'
  set_test_key()
  with_mocked_bindings({
    df <- eolas_get("nz_cpi", limit = 2L)
    expect_equal(nrow(df), 2L)
    expect_equal(as.character(df$date), c("2024-01-01", "2025-01-01"))
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) {
    url <- httr2::req_get_url(req)
    if (grepl("/data($|\\?)", url)) httr2_mock_resp(body) else httr2_mock_resp(MOCK_DATASET_INFO)
  },
  .package = "eolas")
})

test_that("eolas_get sorts rows by date (API streams in file order)", {
  unsorted_body <- '{"data":[
    {"date":"2023-04-01","period":"2023Q2","value":101.5},
    {"date":"2022-01-01","period":"2022Q1","value":99.0},
    {"date":"2023-01-01","period":"2023Q1","value":100.0}
  ]}'
  with_mock_eolas(unsorted_body, code = {
    df <- eolas_get("nz_cpi")
    expect_equal(
      as.character(df$date),
      c("2022-01-01", "2023-01-01", "2023-04-01")
    )
  })
})

# ---------------------------------------------------------------------------
# eolas_get_* source functions
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
    # cli print methods write to the message connection as well as stdout.
    output <- c(
      capture.output(print(df)),
      capture.output(print(df), type = "message")
    )
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
# Empty-body / gateway-error handling (CF 504/521/522)
# ---------------------------------------------------------------------------

# Helper: construct an httr2_response with an empty (0-byte) body.  The normal
# httr2_mock_resp() always supplies a JSON body; this one deliberately does not
# so we can exercise the innermost tryCatch fallback in eolas_check_status.
test_that("504 with empty body produces a clean error message (not an internal traceback)", {
  set_test_key()
  with_mocked_bindings(
    {
      err <- tryCatch(eolas_get("nz_cpi"), error = function(e) e)
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err), "504")
      expect_false(grepl("Can't retrieve empty body", conditionMessage(err), fixed = TRUE))
    },
    eolas_http_perform = function(...) httr2_mock_resp_empty(504L),
    .package = "eolas"
  )
})

test_that("521 with empty body produces a clean error message", {
  set_test_key()
  with_mocked_bindings(
    {
      err <- tryCatch(eolas_get("nz_cpi"), error = function(e) e)
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err), "521")
      expect_false(grepl("Can't retrieve empty body", conditionMessage(err), fixed = TRUE))
    },
    eolas_http_perform = function(...) httr2_mock_resp_empty(521L),
    .package = "eolas"
  )
})

test_that("522 with empty body produces a clean error message", {
  set_test_key()
  with_mocked_bindings(
    {
      err <- tryCatch(eolas_get("nz_cpi"), error = function(e) e)
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err), "522")
    },
    eolas_http_perform = function(...) httr2_mock_resp_empty(522L),
    .package = "eolas"
  )
})

test_that("empty-body error detail includes retry hint", {
  set_test_key()
  with_mocked_bindings(
    {
      err <- tryCatch(eolas_get("nz_cpi"), error = function(e) e)
      expect_match(conditionMessage(err), "retry", ignore.case = TRUE)
    },
    eolas_http_perform = function(...) httr2_mock_resp_empty(504L),
    .package = "eolas"
  )
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
    expect_s3_class(df, "tbl_df")
    expect_s3_class(df, "eolas_dataset")
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
