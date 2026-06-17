library(testthat)

INFO_OECD_CPI <- '{"name":"nz_cpi","title":"NZ CPI inflation (annual % change)","source":"OECD",
  "namespace":"oecd","description":"OECD YoY %.","refresh_cadence":"monthly",
  "columns":[{"name":"date","type":"date","description":"Observation date"},
             {"name":"value","type":"double","description":"Value"}]}'

DATA_BODY <- '{"data":[{"date":"2023-01-01","period":"2023Q1","value":1.5}]}'

test_that("eolas_get_statsnz warns when nz_cpi is OECD-sourced", {
  ns <- getNamespace("eolas")
  old_cache <- as.list(ns$.eolas_meta_cache, all.names = TRUE)
  rm(list = ls(envir = ns$.eolas_meta_cache), envir = ns$.eolas_meta_cache)
  on.exit({
    rm(list = ls(envir = ns$.eolas_meta_cache), envir = ns$.eolas_meta_cache)
    for (nm in names(old_cache)) assign(nm, old_cache[[nm]], envir = ns$.eolas_meta_cache)
  }, add = TRUE)

  set_test_key()
  with_mocked_bindings({
    expect_warning(
      df <- eolas_get_statsnz("nz_cpi", meta = TRUE),
      "OECD"
    )
    expect_equal(attr(df, "eolas_source"), "Stats NZ")
    expect_equal(eolas_meta(df)$source[[1]], "OECD")
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) {
    url <- httr2::req_get_url(req)
    if (grepl("/data($|\\?)", url)) httr2_mock_resp(DATA_BODY) else httr2_mock_resp(INFO_OECD_CPI)
  },
  .package = "eolas")
})