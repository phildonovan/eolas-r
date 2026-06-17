library(testthat)

INFO_BODY <- '{"name":"nz_cpi","title":"NZ Consumer Price Index","source":"Stats NZ",
  "namespace":"statsnz","description":"Official quarterly CPI from Stats NZ.",
  "refresh_cadence":"quarterly",
  "columns":[
    {"name":"date","type":"date","description":"Observation date"},
    {"name":"value","type":"double","description":"Index value"}
  ]}'

DATA_BODY <- '{"data":[
  {"date":"2023-01-01","period":"2023Q1","value":100.0},
  {"date":"2023-04-01","period":"2023Q2","value":101.5}
]}'

mock_info_and_data <- function(code) {
  ns <- getNamespace("eolas")
  old_cache <- as.list(ns$.eolas_meta_cache, all.names = TRUE)
  rm(list = ls(envir = ns$.eolas_meta_cache), envir = ns$.eolas_meta_cache)
  on.exit({
    rm(list = ls(envir = ns$.eolas_meta_cache), envir = ns$.eolas_meta_cache)
    for (nm in names(old_cache)) assign(nm, old_cache[[nm]], envir = ns$.eolas_meta_cache)
  }, add = TRUE)

  set_test_key()
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        httr2_mock_resp(DATA_BODY)
      } else {
        httr2_mock_resp(INFO_BODY)
      }
    },
    .package = "eolas"
  )
}

test_that("eolas_get attaches eolas_meta and eolas_columns", {
  mock_info_and_data({
    df <- eolas_get("nz_cpi")
    meta <- attr(df, "eolas_meta")
    cols <- attr(df, "eolas_columns")
    expect_s3_class(meta, "tbl_df")
    expect_equal(meta$title[[1]], "NZ Consumer Price Index")
    expect_equal(meta$description[[1]], "Official quarterly CPI from Stats NZ.")
    expect_s3_class(cols, "tbl_df")
    expect_equal(cols$name, c("date", "value"))
  })
})

test_that("eolas_meta() and eolas_column_label() accessors work", {
  mock_info_and_data({
    df <- eolas_get("nz_cpi")
    expect_equal(eolas_meta(df)$refresh_cadence[[1]], "quarterly")
    expect_equal(eolas_column_label(df, "value"), "Index value")
    expect_null(eolas_column_label(df, "missing_col"))
  })
})

test_that("print.eolas_dataset shows title subtitle not full description", {
  mock_info_and_data({
    df <- eolas_get_statsnz("nz_cpi")
    output <- c(
      capture.output(print(df)),
      capture.output(print(df), type = "message")
    )
    expect_true(any(grepl("NZ Consumer Price Index", output)))
    expect_true(any(grepl("refreshed quarterly", output)))
    expect_false(any(grepl("Official quarterly CPI from Stats NZ", output)))
  })
})

test_that("eolas_get_statsnz passes meta = TRUE through", {
  mock_info_and_data({
    df <- eolas_get_statsnz("nz_cpi", meta = TRUE)
    expect_false(is.null(attr(df, "eolas_meta")))
    expect_equal(eolas_meta(df)$title[[1]], "NZ Consumer Price Index")
  })
})

test_that("meta = FALSE skips metadata attachment", {
  ns <- getNamespace("eolas")
  old_cache <- as.list(ns$.eolas_meta_cache, all.names = TRUE)
  on.exit({
    rm(list = ls(envir = ns$.eolas_meta_cache), envir = ns$.eolas_meta_cache)
    for (nm in names(old_cache)) assign(nm, old_cache[[nm]], envir = ns$.eolas_meta_cache)
  }, add = TRUE)

  with_mock_eolas(DATA_BODY, code = {
    with_mocked_bindings({
      df <- eolas_get("nz_cpi", meta = FALSE)
      expect_null(attr(df, "eolas_meta"))
      expect_null(attr(df, "eolas_columns"))
    },
    eolas_info = function(...) stop("eolas_info should not be called", call. = FALSE),
    .package = "eolas")
  })
})