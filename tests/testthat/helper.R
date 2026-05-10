library(httptest2)
library(testthat)

# Point all requests at a local mock server path
set_mock_dir <- function() {
  httptest2::use_mock_api()
}


# ---------------------------------------------------------------------------
# httr2 response mocking — shared across test files
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
