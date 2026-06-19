# Arrow wire-format negotiation (build-specs/api-arrow-format.md, Phase 3).

reset_arrow_runtime <- function() {
  ns <- getNamespace("eolas")
  rm(list = ls(ns$.eolas_runtime), envir = ns$.eolas_runtime)
}

test_that(".eolas_nag_arrow_once messages exactly once per session", {
  reset_arrow_runtime()
  ns <- getNamespace("eolas")
  nag <- get(".eolas_nag_arrow_once", envir = ns)
  expect_message(nag(), "install.packages\\(\"arrow\"\\)")
  expect_silent(nag())  # memoised — second call is a no-op
})

test_that(".eolas_fetch_df uses the Arrow path when the server returns Arrow", {
  skip_if_not_installed("arrow")
  reset_arrow_runtime()
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  want <- data.frame(date = c("2023-01-01", "2023-04-01"),
                     period = c("2023Q1", "2023Q2"),
                     value = c(100, 101.5))
  ipc <- arrow::write_to_raw(arrow::arrow_table(want), format = "stream")
  arrow_resp <- structure(
    list(
      method = "GET", url = "https://api.eolas.fyi/test", status_code = 200L,
      headers = structure(
        list(`content-type` = "application/vnd.apache.arrow.stream"),
        class = "httr2_headers"),
      body = ipc,
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
  with_mocked_bindings(
    {
      fetch <- get(".eolas_fetch_df", envir = ns)
      fetched <- fetch("nz_cpi", list(limit = 0L), EOLAS_BASE_URL)
      df <- fetched$df
      expect_equal(nrow(df), 2L)
      expect_equal(sort(names(df)), c("date", "period", "value"))
      expect_true(isTRUE(ns$.eolas_runtime$arrow_supported))
    },
    eolas_http_perform = function(...) arrow_resp,
    .package = "eolas"
  )
})

test_that("a JSON server response falls back cleanly and memoises no-arrow", {
  reset_arrow_runtime()
  with_mock_eolas('{"data":[{"date":"2023-01-01","period":"2023Q1","value":100}]}', code = {
    df <- eolas_get("nz_cpi")
    expect_s3_class(df, "eolas_dataset")
    expect_equal(nrow(df), 1L)
    ns <- getNamespace("eolas")
    # Server (mock) returned JSON for format=arrow -> remembered as unsupported
    expect_false(isTRUE(ns$.eolas_runtime$arrow_supported))
  })
})
