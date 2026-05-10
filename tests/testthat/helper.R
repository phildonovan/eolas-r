library(httptest2)

# Point all requests at a local mock server path
set_mock_dir <- function() {
  httptest2::use_mock_api()
}
