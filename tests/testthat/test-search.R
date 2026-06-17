library(testthat)

DATASET_LIST_BODY <- '[
  {"name":"rbnz_b2_wholesale_rates_monthly","title":"Wholesale rates","source":"RBNZ","namespace":"rbnz","description":"OCR and bank bills"},
  {"name":"nz_gdp","title":"NZ GDP","source":"OECD","namespace":"oecd","description":"Growth"}
]'

test_that("eolas_search expands OCR alias", {
  set_test_key()
  with_mocked_bindings({
    out <- eolas_search("OCR")
    expect_equal(out$name, "rbnz_b2_wholesale_rates_monthly")
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp(DATASET_LIST_BODY),
  .package = "eolas")
})

test_that("eolas_search filters by source", {
  set_test_key()
  with_mocked_bindings({
    out <- eolas_search("OCR", source = "RBNZ")
    expect_equal(nrow(out), 1L)
    out2 <- eolas_search("OCR", source = "OECD")
    expect_equal(nrow(out2), 0L)
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp(DATASET_LIST_BODY),
  .package = "eolas")
})