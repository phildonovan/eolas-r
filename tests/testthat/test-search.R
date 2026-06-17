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

WELLINGTON_BODY <- '[
  {"name":"kcdc_flood_extents","title":"Flood extents","source":"Wellington Region Councils"},
  {"name":"wcc_flood_hazard_operative","title":"WCC floods","source":"Wellington Region Councils"},
  {"name":"nz_gdp","title":"NZ GDP","source":"OECD","description":"kapiti in description only"}
]'

test_that("eolas_search expands kapiti alias to kcdc_* names", {
  set_test_key()
  with_mocked_bindings({
    out <- eolas_search("kapiti")
    expect_equal(out$name, "kcdc_flood_extents")
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp(WELLINGTON_BODY),
  .package = "eolas")
})

test_that("eolas_search expands porirua alias to pcc_* names", {
  set_test_key()
  with_mocked_bindings({
    out <- eolas_search("porirua")
    expect_equal(out$name, "pcc_district_plan_zones")
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp('[
    {"name":"pcc_district_plan_zones","title":"District plan","source":"Wellington Region Councils"},
    {"name":"wcc_district_plan_zones_2024","title":"WCC zones","source":"Wellington Region Councils"}
  ]'),
  .package = "eolas")
})