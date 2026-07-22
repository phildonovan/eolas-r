library(testthat)

# eolas_get(geometry = FALSE) -- omit the geometry_wkt column.
#
# Tester feedback (Aaron, 2026-07-22): pulling TA/RC data for analysis drags a
# geometry_wkt column that dwarfs the attributes. 1017/1536 datasets carry it.
# The API projects the column away at the Iceberg scan, so it is never read from
# storage; the client's job is to (a) send the parameter and (b) stop mirroring
# the server's 413 geometry trigger, which would otherwise keep routing these
# calls to a bulk download and defeat the whole point.

# A spatial dataset: geometry present, small enough that ONLY geometry trips
# the large-dataset guard.
GEO_META <- paste0(
  '{"name":"nz_ta_2023","title":"TAs","source":"Stats NZ Geospatial",',
  '"namespace":"statsnz_geo","has_geometry":true,"geometry_type":"polygon",',
  '"bulk_export_class":"materialised","row_count_at_last_refresh":67}'
)

ROWS <- '{"data":[{"ta_name":"Auckland","population":1695200}]}'

# Mock that records every /data URL the client builds.
with_url_capture <- function(code, meta_body = GEO_META, rows = ROWS) {
  set_test_key()
  seen <- new.env(parent = emptyenv())
  seen$data_urls <- character()
  res <- with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        seen$data_urls <- c(seen$data_urls, url)
        httr2_mock_resp(rows)
      } else {
        httr2_mock_resp(meta_body)
      }
    },
    .package = "eolas"
  )
  list(result = res, urls = seen$data_urls)
}


# NOTE: the live path tries format=arrow first and falls back to JSON. Against
# these mocks (which answer JSON to both) that shows up as two /data requests,
# where a real call makes one. Assert over ALL captured URLs rather than pinning
# a count, so the tests describe the parameter, not the transport's retry shape.

test_that("geometry = FALSE sends geometry=false on the data request", {
  out <- with_url_capture(
    eolas_get("nz_ta_2023", limit = 10, geometry = FALSE)
  )
  expect_gte(length(out$urls), 1L)
  expect_true(all(grepl("geometry=false", out$urls, fixed = TRUE)))
})


test_that("the default sends no geometry parameter at all", {
  # geometry=true IS the server default; sending it would churn URLs (and any
  # CDN cache keys) on the overwhelming majority of calls for no benefit.
  out <- with_url_capture(
    eolas_get("nz_ta_2023", limit = 10)
  )
  expect_gte(length(out$urls), 1L)
  expect_false(any(grepl("geometry", out$urls, fixed = TRUE)))
})


test_that("geometry = FALSE lets a whole-dataset pull stay on the live path", {
  # Without threading `geometry` into the routing mirror, this dataset (geometry
  # present, bulk-exportable) would be diverted to eolas_get_local() and never
  # hit /data at all -- the regression this test exists to catch.
  out <- with_url_capture(
    eolas_get("nz_ta_2023", geometry = FALSE)
  )
  expect_gte(length(out$urls), 1L)
  expect_true(all(grepl("geometry=false", out$urls, fixed = TRUE)))
})


test_that("geometry = TRUE still routes a whole-dataset spatial pull to bulk", {
  # The existing behaviour must be untouched when geometry is not narrowed.
  routed <- FALSE
  with_mocked_bindings(
    {
      set_test_key()
      with_mocked_bindings(
        try(eolas_get("nz_ta_2023"), silent = TRUE),
        .eolas_use_streaming = function() FALSE,
        eolas_http_perform = function(req) httr2_mock_resp(GEO_META),
        .package = "eolas"
      )
    },
    eolas_get_local = function(...) {
      routed <<- TRUE
      data.frame()
    },
    .package = "eolas"
  )
  expect_true(routed)
})


test_that("geometry = FALSE with as_sf = TRUE is rejected", {
  expect_error(
    eolas_get("nz_ta_2023", geometry = FALSE, as_sf = TRUE),
    "contradictory"
  )
})


test_that(".eolas_live_pull_blocked keeps the row-count trigger when geometry=FALSE", {
  # Dropping a column doesn't reduce row count, so a genuinely huge table must
  # still be blocked even with geometry = FALSE.
  big <- data.frame(
    has_geometry = TRUE,
    row_count_at_last_refresh = 5e6,
    stringsAsFactors = FALSE
  )
  small <- data.frame(
    has_geometry = TRUE,
    row_count_at_last_refresh = 67,
    stringsAsFactors = FALSE
  )
  expect_true(eolas:::.eolas_live_pull_blocked(big, geometry = FALSE))
  expect_false(eolas:::.eolas_live_pull_blocked(small, geometry = FALSE))
  # ...and geometry alone still blocks when geometry is requested.
  expect_true(eolas:::.eolas_live_pull_blocked(small, geometry = TRUE))
})


# ---- bulk-route path (Grok review, 2026-07-22) -----------------------------
# A spatial table ALSO over the row-count threshold stays "blocked" even with
# geometry = FALSE, so eolas_get() routes it to the bulk cache. The flag must
# survive that hand-off, or the caller silently receives the full
# geometry-bearing file -- and with as_sf = NULL, an auto-converted sf object.

BIG_GEO_META <- paste0(
  '{"name":"nz_parcels","title":"Parcels","source":"LINZ","namespace":"linz",',
  '"has_geometry":true,"geometry_type":"polygon",',
  '"bulk_export_class":"materialised","row_count_at_last_refresh":2000000}'
)

test_that("bulk-routed eolas_get(geometry = FALSE) strips the geometry column", {
  seen <- list()
  res <- with_mocked_bindings(
    {
      set_test_key()
      with_mocked_bindings(
        eolas_get("nz_parcels", geometry = FALSE),
        .eolas_use_streaming = function() FALSE,
        eolas_http_perform = function(req) httr2_mock_resp(BIG_GEO_META),
        .package = "eolas"
      )
    },
    eolas_get_local = function(name, as_sf = NULL, ...) {
      seen$as_sf <<- as_sf
      data.frame(
        parcel_id = 1L,
        geometry_wkt = "POINT(174 -36)",
        stringsAsFactors = FALSE
      )
    },
    .package = "eolas"
  )
  expect_false("geometry_wkt" %in% names(res))
  expect_true("parcel_id" %in% names(res))
  # Must not hand back an sf object either.
  expect_false(isTRUE(seen$as_sf))
})

test_that("bulk-routed eolas_get() keeps geometry by default", {
  res <- with_mocked_bindings(
    {
      set_test_key()
      with_mocked_bindings(
        eolas_get("nz_parcels"),
        .eolas_use_streaming = function() FALSE,
        eolas_http_perform = function(req) httr2_mock_resp(BIG_GEO_META),
        .package = "eolas"
      )
    },
    eolas_get_local = function(name, as_sf = NULL, ...) {
      data.frame(
        parcel_id = 1L,
        geometry_wkt = "POINT(174 -36)",
        stringsAsFactors = FALSE
      )
    },
    .package = "eolas"
  )
  expect_true("geometry_wkt" %in% names(res))
})
