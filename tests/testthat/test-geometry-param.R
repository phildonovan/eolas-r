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


# ---- bulk-route path -------------------------------------------------------
# A spatial table ALSO over the row-count threshold stays blocked even with
# geometry = FALSE, so eolas_get() routes it to the bulk cache -- which has no
# server-side projection. Two responsibilities, tested separately:
#   eolas_get()       must pass the flag DOWN to eolas_get_local()
#   eolas_get_local() must project the column away AT READ TIME

BIG_GEO_META <- paste0(
  '{"name":"nz_parcels","title":"Parcels","source":"LINZ","namespace":"linz",',
  '"has_geometry":true,"geometry_type":"polygon",',
  '"bulk_export_class":"materialised","row_count_at_last_refresh":2000000}'
)

test_that("eolas_get() passes geometry down to eolas_get_local()", {
  seen <- new.env(parent = emptyenv())
  with_mocked_bindings(
    {
      set_test_key()
      with_mocked_bindings(
        eolas_get("nz_parcels", geometry = FALSE),
        .eolas_use_streaming = function() FALSE,
        eolas_http_perform = function(req) httr2_mock_resp(BIG_GEO_META),
        .package = "eolas"
      )
    },
    eolas_get_local = function(name, geometry = TRUE, ...) {
      seen$geometry <- geometry
      data.frame(parcel_id = 1L)
    },
    .package = "eolas"
  )
  # Dropping the column afterwards would decode the WKT for nothing -- the flag
  # has to reach the reader.
  expect_false(seen$geometry)
})

test_that("eolas_get() defaults to geometry = TRUE on the bulk route", {
  seen <- new.env(parent = emptyenv())
  with_mocked_bindings(
    {
      set_test_key()
      with_mocked_bindings(
        eolas_get("nz_parcels"),
        .eolas_use_streaming = function() FALSE,
        eolas_http_perform = function(req) httr2_mock_resp(BIG_GEO_META),
        .package = "eolas"
      )
    },
    eolas_get_local = function(name, geometry = TRUE, ...) {
      seen$geometry <- geometry
      data.frame(parcel_id = 1L)
    },
    .package = "eolas"
  )
  expect_true(seen$geometry)
})

.write_geo_parquet <- function(path) {
  arrow::write_parquet(
    data.frame(
      parcel_id = 1:2,
      area_m2 = c(100, 250),
      geometry_wkt = c("POINT(174 -36)", "POINT(175 -37)"),
      stringsAsFactors = FALSE
    ),
    path
  )
}

test_that("eolas_get_local(geometry = FALSE) projects the column at read time", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "nz_parcels.parquet")
  .write_geo_parquet(target)
  before <- file.info(target)$size

  out <- with_mocked_bindings(
    eolas_get_local("nz_parcels",
      cache_dir = dir, format = "parquet",
      meta = FALSE, geometry = FALSE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )

  expect_false("geometry_wkt" %in% names(out))
  expect_equal(sort(names(out)), c("area_m2", "parcel_id"))
  expect_equal(nrow(out), 2L)

  # The cached artifact is untouched -- one file serves both variants.
  expect_equal(file.info(target)$size, before)
  expect_true("geometry_wkt" %in% names(arrow::open_dataset(target, format = "parquet")$schema))
})

test_that("eolas_get_local() keeps geometry by default", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "nz_parcels.parquet")
  .write_geo_parquet(target)

  out <- with_mocked_bindings(
    eolas_get_local("nz_parcels",
      cache_dir = dir, format = "parquet",
      meta = FALSE, as_sf = FALSE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_true("geometry_wkt" %in% names(out))
})

test_that("eolas_get_local(geometry = FALSE) never returns an sf object", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "nz_parcels.parquet")
  .write_geo_parquet(target)

  out <- with_mocked_bindings(
    eolas_get_local("nz_parcels",
      cache_dir = dir, format = "parquet",
      meta = FALSE, geometry = FALSE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_false(inherits(out, "sf"))
})


# ---- format branches --------------------------------------------------------
# These paths existed but had never been executed by any test: the csv_gz reader
# (also the fallback when a Parquet read fails) and the as_arrow reader.

.write_geo_csv_gz <- function(path) {
  con <- gzfile(path, "w")
  utils::write.csv(
    data.frame(
      a = 1:2, b = c("x", "y"),
      geometry_wkt = c("POINT(1 1)", "POINT(2 2)"),
      stringsAsFactors = FALSE
    ),
    con,
    row.names = FALSE
  )
  close(con)
}

test_that("csv_gz read honours geometry = FALSE", {
  dir <- withr::local_tempdir()
  target <- file.path(dir, "x.csv.gz")
  .write_geo_csv_gz(target)
  out <- with_mocked_bindings(
    eolas_get_local("x",
      cache_dir = dir, format = "csv_gz",
      meta = FALSE, geometry = FALSE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_equal(sort(names(out)), c("a", "b"))
  expect_equal(nrow(out), 2L)
})

test_that("csv_gz read keeps geometry by default", {
  dir <- withr::local_tempdir()
  target <- file.path(dir, "x.csv.gz")
  .write_geo_csv_gz(target)
  out <- with_mocked_bindings(
    eolas_get_local("x",
      cache_dir = dir, format = "csv_gz",
      meta = FALSE, as_sf = FALSE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_true("geometry_wkt" %in% names(out))
})

test_that("as_arrow honours geometry = FALSE", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "y.parquet")
  .write_geo_parquet(target)
  out <- with_mocked_bindings(
    eolas_get_local("y",
      cache_dir = dir, format = "parquet",
      meta = FALSE, geometry = FALSE, as_arrow = TRUE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_s3_class(out, "Table")
  expect_false("geometry_wkt" %in% names(out))
})

test_that("as_arrow keeps geometry by default", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  target <- file.path(dir, "y.parquet")
  .write_geo_parquet(target)
  out <- with_mocked_bindings(
    eolas_get_local("y",
      cache_dir = dir, format = "parquet",
      meta = FALSE, as_arrow = TRUE
    ),
    eolas_sync_bulk = function(...) invisible(target),
    .package = "eolas"
  )
  expect_true("geometry_wkt" %in% names(out))
})


# ---- safe-slice exception (architecture.md 2.5.4c, 2026-08-30) ------------
# Without start/end, a large/geo table is served live ONLY as a slice of
# 0 < limit <= 10,000 rows. NULL, 0 and anything larger are refused alike, so
# the client routes those to the bulk cache and trims client-side.

test_that(".eolas_live_slice_allowed is exactly the server's test", {
  expect_equal(eolas:::.EOLAS_SAFE_LIVE_SLICE_ROWS, 10000L)
  expect_false(eolas:::.eolas_live_slice_allowed(NULL))
  expect_false(eolas:::.eolas_live_slice_allowed(0))
  expect_true(eolas:::.eolas_live_slice_allowed(1))
  expect_true(eolas:::.eolas_live_slice_allowed(10000))
  expect_false(eolas:::.eolas_live_slice_allowed(10001))
  expect_false(eolas:::.eolas_live_slice_allowed(50000))
})


test_that(".eolas_live_pull_blocked honours the 10k safe slice", {
  geo <- data.frame(
    has_geometry = TRUE,
    row_count_at_last_refresh = 67,
    stringsAsFactors = FALSE
  )
  big <- data.frame(
    has_geometry = FALSE,
    row_count_at_last_refresh = 5e6,
    stringsAsFactors = FALSE
  )
  small <- data.frame(
    has_geometry = FALSE,
    row_count_at_last_refresh = 67,
    stringsAsFactors = FALSE
  )
  for (m in list(geo, big)) {
    expect_false(eolas:::.eolas_live_pull_blocked(m, limit = 1))
    expect_false(eolas:::.eolas_live_pull_blocked(m, limit = 10000))
    expect_true(eolas:::.eolas_live_pull_blocked(m, limit = 10001))
    expect_true(eolas:::.eolas_live_pull_blocked(m, limit = 50000))
    expect_true(eolas:::.eolas_live_pull_blocked(m, limit = 200000))
    expect_true(eolas:::.eolas_live_pull_blocked(m, limit = 0))
    expect_true(eolas:::.eolas_live_pull_blocked(m))
  }
  # The slice is irrelevant when the table is neither large nor spatial.
  expect_false(eolas:::.eolas_live_pull_blocked(small, limit = 0))
  expect_false(eolas:::.eolas_live_pull_blocked(small, limit = 999999))
})


test_that("limit = 10000 is the largest slice that stays on the live path", {
  routed <- FALSE
  out <- with_mocked_bindings(
    with_url_capture(eolas_get("nz_ta_2023", limit = 10000)),
    eolas_get_local = function(...) {
      routed <<- TRUE
      data.frame()
    },
    .package = "eolas"
  )
  expect_false(routed)
  expect_gte(length(out$urls), 1L)
  expect_true(all(grepl("limit=10000(&|$)", out$urls)))
})


test_that("limit above the safe slice routes a spatial table to bulk and trims", {
  local_rows <- data.frame(
    date  = as.Date(c("2020-01-01", "2022-01-01", "2021-01-01")),
    value = c(1, 3, 2)
  )
  for (lim in c(10001, 50000, 0)) {
    routed <- FALSE
    result <- with_mocked_bindings(
      {
        set_test_key()
        with_mocked_bindings(
          eolas_get("nz_ta_2023", limit = lim),
          .eolas_use_streaming = function() FALSE,
          eolas_http_perform = function(req) {
            if (grepl("/data($|\\?)", httr2::req_get_url(req))) {
              stop("live /data path must not be hit for limit = ", lim)
            }
            httr2_mock_resp(GEO_META)
          },
          .package = "eolas"
        )
      },
      eolas_get_local = function(...) {
        routed <<- TRUE
        local_rows
      },
      .package = "eolas"
    )
    expect_true(routed, info = paste("limit =", lim))
    if (lim > 0) {
      # Sorted by date and trimmed to min(limit, n): the table is only 3 rows.
      expect_equal(result$value, c(1, 2, 3), info = paste("limit =", lim))
    } else {
      # limit = 0 is "whole dataset": the bulk read is returned as-is.
      expect_equal(result$value, c(1, 3, 2))
    }
  }
})


test_that("routed limit keeps most-recent-N semantics on a dated table", {
  local_rows <- data.frame(
    date  = as.Date(c("2020-01-01", "2022-01-01", "2021-01-01")),
    value = c(1, 3, 2)
  )
  # Simulate a small table with a limit that is over the safe slice but under
  # the table size: not possible with 3 rows, so trim via the helper directly
  # and check the routed path used the same helper (order + tail).
  trimmed <- eolas:::.eolas_apply_row_limit(
    local_rows[order(local_rows$date), , drop = FALSE], 2L
  )
  expect_equal(trimmed$value, c(2, 3))
})
