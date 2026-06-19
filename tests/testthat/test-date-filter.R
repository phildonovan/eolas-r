library(testthat)

test_that(".eolas_date_filter_column reads explicit API field", {
  info <- tibble::tibble(
    name = "nz_cpi",
    date_filter_column = list("date")
  )
  expect_equal(.eolas_date_filter_column(info), "date")
})


test_that(".eolas_date_filter_column infers from columns glossary", {
  col_df <- tibble::tibble(
    name = c("awarded_date", "amount"),
    type = c("string", "double")
  )
  info <- tibble::tibble(name = "gets_awards", columns = list(col_df))
  expect_equal(.eolas_date_filter_column(info), "awarded_date")
})


test_that(".eolas_resolve_date_bounds strips non-temporal bounds", {
  info <- tibble::tibble(name = "nz_parcels", date_filter_column = list(NA_character_))
  out <- .eolas_resolve_date_bounds(info, "2020-01-01", NULL)
  expect_null(out$start)
  expect_null(out$end)
  expect_true(out$stripped)
})


test_that("eolas_get warns and routes to get_local when start is ignored on geo", {
  geo_body <- list(
    name = "nz_addresses",
    namespace = "linz",
    bulk_export_class = "materialised",
    has_geometry = TRUE,
    geometry_type = "point",
    row_count_at_last_refresh = 2418264L,
    date_filter_column = NULL,
    columns = list(
      list(name = "address_id", type = "string"),
      list(name = "geometry_wkt", type = "string")
    )
  )
  geo_info <- eolas:::.eolas_parse_info_response(geo_body)
  sentinel <- data.frame(address_id = 1L, geometry_wkt = "POINT (0 0)")

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    .eolas_info_cached = function(n, base_url = NULL) geo_info,
    eolas_get_local = function(name, ...) sentinel,
    .package = "eolas"
  )

  expect_warning(
    result <- eolas_get("nz_addresses", start = "2020-01-01"),
    "start/end ignored"
  )
  expect_equal(result, sentinel)
})