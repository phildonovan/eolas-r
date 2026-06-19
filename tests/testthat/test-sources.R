# Parametrised smoke tests for every eolas_get_* / eolas_list_* wrapper in R/sources.R.
# Each wrapper is a thin delegate; this exercises them all without duplicating per-source cases.

test_that("every eolas_get_* wrapper tags results with its source label", {
  catalog <- source_wrapper_catalog()
  expect_length(catalog$gets, 35L)

  with_mock_eolas(SOURCE_DATA_BODY, code = {
    for (fn in names(catalog$gets)) {
      result <- suppressWarnings(do.call(fn, list(name = "nz_cpi")))
      expect_equal(
        attr(result, "eolas_source"),
        catalog$gets[[fn]],
        info = fn
      )
    }
  })
})

test_that("every eolas_list_* wrapper filters eolas_list() by source", {
  catalog <- source_wrapper_catalog()
  expect_length(catalog$lists, 34L)

  for (fn in names(catalog$lists)) {
    row <- list(
      name = "ds_test", title = "Test dataset", source = catalog$lists[[fn]],
      namespace = "test_ns", description = ""
    )
    body <- jsonlite::toJSON(list(row), auto.unbox = TRUE)
    with_mock_eolas(body, code = {
      result <- do.call(fn, list())
      expect_equal(nrow(result), 1L, info = fn)
      expect_equal(result$source[[1L]], catalog$lists[[fn]], info = fn)
    })
  }
})

test_that("eolas_list_statsnz_geo filters on namespace not source label", {
  body <- jsonlite::toJSON(list(list(
    name = "geo_ds", title = "Geo layer", source = "Stats NZ",
    namespace = "statsnz_geo", description = ""
  )), auto.unbox = TRUE)
  with_mock_eolas(body, code = {
    result <- eolas_list_statsnz_geo()
    expect_equal(nrow(result), 1L)
    expect_equal(result$namespace[[1L]], "statsnz_geo")
    expect_equal(result$name[[1L]], "geo_ds")
  })
})