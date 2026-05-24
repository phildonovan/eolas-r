library(testthat)
library(withr)

# Tests for eolas_get() smart-routing: mode="auto" / "live" / "cached"
# and eolas_get_local() as an alias for mode="cached".

# ---------------------------------------------------------------------------
# Shared metadata shapes
# ---------------------------------------------------------------------------

META_SMALL_TABULAR <- list(
  name                     = "nz_cpi",
  source                   = "Stats NZ",
  namespace                = "statsnz",
  bulk_export_class        = "cc_by",
  row_count_at_last_refresh = 145L   # below 100,000 threshold
)

META_LARGE_TABULAR <- list(
  name                     = "pharmac_schedule",
  source                   = "PHARMAC",
  namespace                = "pharmac",
  bulk_export_class        = "cc_by",
  row_count_at_last_refresh = 150000L  # above threshold
)

META_GEO <- list(
  name                     = "nz_parcels",
  source                   = "LINZ",
  namespace                = "linz",
  bulk_export_class        = "cc_by",
  geometry_type            = "MultiPolygon",
  row_count_at_last_refresh = 3000000L
)

META_GEO_SMALL_COUNT <- list(
  name                     = "nz_parcels",
  source                   = "LINZ",
  namespace                = "linz",
  bulk_export_class        = "cc_by",
  geometry_type            = "MultiPolygon",
  row_count_at_last_refresh = 5000L  # small count but geo -> still cache
)

META_LICENCE_RESTRICTED <- list(
  name                     = "oecd_gdp",
  source                   = "OECD",
  namespace                = "oecd",
  bulk_export_class        = "none",  # licence floor
  row_count_at_last_refresh = 250000L
)

FAKE_LOCAL_DF <- data.frame(date = "2023-01-01", value = 1100.5,
                             stringsAsFactors = FALSE)

FAKE_LIVE_DF <- data.frame(date = "2023-01-01", value = 99.0,
                            stringsAsFactors = FALSE)

# Helper that seeds the API key env so eolas_get() doesn't bail on missing key.
.seed_key <- function() {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)
}

# ---------------------------------------------------------------------------
# mode = "auto" — small tabular dataset stays on live path
# ---------------------------------------------------------------------------

test_that("auto mode: small non-geo bulk-eligible dataset routes to live", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_SMALL_TABULAR
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_cpi")

  expect_equal(info_calls,  1L)   # metadata was checked
  expect_equal(local_calls, 0L)   # cache path NOT taken
  expect_equal(fetch_calls, 1L)   # live path taken
})

# ---------------------------------------------------------------------------
# mode = "auto" — large tabular dataset routes to cache
# ---------------------------------------------------------------------------

test_that("auto mode: large bulk-eligible dataset routes to cache+sync", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_LARGE_TABULAR
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("pharmac_schedule")

  expect_equal(info_calls,  1L)
  expect_equal(local_calls, 1L)   # cache path taken
  expect_equal(fetch_calls, 0L)   # live path NOT taken
})

# ---------------------------------------------------------------------------
# mode = "auto" — geo dataset routes to cache regardless of row count
# ---------------------------------------------------------------------------

test_that("auto mode: geo dataset routes to cache+sync even when row count is small", {
  .seed_key()

  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      META_GEO_SMALL_COUNT
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_parcels")

  expect_equal(local_calls, 1L)
  expect_equal(fetch_calls, 0L)
})

# ---------------------------------------------------------------------------
# mode = "auto" — licence-restricted always goes live
# ---------------------------------------------------------------------------

test_that("auto mode: licence-restricted dataset (bulk_export_class='none') routes to live", {
  .seed_key()

  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      META_LICENCE_RESTRICTED
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("oecd_gdp")

  expect_equal(local_calls, 0L)   # cache path must NOT be taken (would 403)
  expect_equal(fetch_calls, 1L)
})

# ---------------------------------------------------------------------------
# Slice args force live path (no metadata call)
# ---------------------------------------------------------------------------

test_that("limit= forces live path without metadata call", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_GEO
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_parcels", limit = 10L)

  expect_equal(info_calls,  0L)   # no metadata round-trip
  expect_equal(local_calls, 0L)
  expect_equal(fetch_calls, 1L)
})

test_that("start= forces live path without metadata call", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_GEO
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_cpi", start = "2020-01-01")

  expect_equal(info_calls,  0L)
  expect_equal(local_calls, 0L)
  expect_equal(fetch_calls, 1L)
})

test_that("end= forces live path without metadata call", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_GEO
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_cpi", end = "2024-12-31")

  expect_equal(info_calls,  0L)
  expect_equal(local_calls, 0L)
  expect_equal(fetch_calls, 1L)
})

# ---------------------------------------------------------------------------
# mode = "live" — always live regardless of dataset properties
# ---------------------------------------------------------------------------

test_that("mode='live' bypasses smart routing entirely", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_GEO
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_parcels", mode = "live")

  expect_equal(info_calls,  0L)
  expect_equal(local_calls, 0L)
  expect_equal(fetch_calls, 1L)
})

# ---------------------------------------------------------------------------
# mode = "cached" — always cache+sync
# ---------------------------------------------------------------------------

test_that("mode='cached' delegates to eolas_get_local without metadata call", {
  .seed_key()

  info_calls  <- 0L
  local_calls <- 0L
  fetch_calls <- 0L

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      info_calls <<- info_calls + 1L
      META_SMALL_TABULAR
    },
    eolas_get_local = function(name, ...) {
      local_calls <<- local_calls + 1L
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      fetch_calls <<- fetch_calls + 1L
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  result <- eolas_get("nz_cpi", mode = "cached")

  expect_equal(info_calls,  0L)
  expect_equal(local_calls, 1L)
  expect_equal(fetch_calls, 0L)
  expect_s3_class(result, "data.frame")
})

# ---------------------------------------------------------------------------
# eolas_get_local is equivalent to mode="cached"
# ---------------------------------------------------------------------------

test_that("eolas_get_local() delegates to eolas_sync_bulk (same code path as mode='cached')", {
  # eolas_get_local calls eolas_sync_bulk + reads the resulting file.
  # Verify that eolas_sync_bulk is called exactly once (= the cache+sync path
  # was taken, not the live path).
  .seed_key()

  sync_call_count <- 0L

  local_mocked_bindings(
    eolas_sync_bulk = function(name, path, format, freshness, base_url = NULL, ...) {
      sync_call_count <<- sync_call_count + 1L
      # Write a stub CSV.gz so the read step succeeds.
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      con <- gzcon(file(path, "wb"))
      writeLines(c("date,value", "2023-01-01,99.0"), con)
      close(con)
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = "snap_test",
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 100L)
    },
    .package = "eolas"
  )

  tmp    <- withr::local_tempdir()
  result <- eolas_get_local("nz_cpi", format = "csv_gz", cache_dir = tmp)

  expect_s3_class(result, "data.frame")
  expect_equal(sync_call_count, 1L)
})

# ---------------------------------------------------------------------------
# auto mode: info() failure falls through to live
# ---------------------------------------------------------------------------

test_that("auto mode: info() error falls through to live path silently", {
  .seed_key()

  live_called  <- FALSE
  local_called <- FALSE

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      stop("network error")
    },
    eolas_get_local = function(name, ...) {
      local_called <<- TRUE
      FAKE_LOCAL_DF
    },
    .eolas_fetch_df = function(name, params, base_url) {
      live_called <<- TRUE
      FAKE_LIVE_DF
    },
    .package = "eolas"
  )

  eolas_get("nz_cpi")

  expect_false(local_called)
  expect_true(live_called)
})

# ---------------------------------------------------------------------------
# invalid mode raises error
# ---------------------------------------------------------------------------

test_that("unknown mode raises an error", {
  .seed_key()
  expect_error(eolas_get("nz_cpi", mode = "turbo"), "should be one of")
})

# ---------------------------------------------------------------------------
# one-time INFO message in auto mode
# ---------------------------------------------------------------------------

test_that("auto mode emits exactly one message per dataset per session", {
  .seed_key()
  ns <- getNamespace("eolas")

  # Use a unique dataset name so it won't have been notified in a prior test.
  test_name <- paste0("nz_parcels_msg_test_", as.integer(Sys.time()))

  # Remove the tracking key if it exists from a prior run in the same session.
  e <- ns$.eolas_auto_route_notified
  if (exists(test_name, envir = e, inherits = FALSE)) {
    rm(list = test_name, envir = e)
  }

  meta_geo_test <- c(META_GEO, list(name = test_name))

  local_mocked_bindings(
    eolas_info = function(name, base_url = NULL) {
      meta_geo_test
    },
    eolas_get_local = function(name, ...) {
      FAKE_LOCAL_DF
    },
    .package = "eolas"
  )

  msgs1 <- capture_messages(eolas_get(test_name))
  msgs2 <- capture_messages(eolas_get(test_name))

  # First call should produce a message; second should not.
  expect_equal(length(msgs1), 1L)
  expect_true(grepl("cache\\+sync", msgs1[1]))
  expect_true(grepl("mode='live'", msgs1[1]))
  expect_equal(length(msgs2), 0L)
})

# ---------------------------------------------------------------------------
# back-compat: existing eolas_get("nz_cpi") calls with only DATA_BODY mocked
# still return an eolas_dataset
# ---------------------------------------------------------------------------

test_that("back-compat: eolas_get with only data endpoint mocked returns eolas_dataset", {
  # with_mock_eolas returns DATA_BODY for ALL http calls.
  # auto mode calls eolas_info -> returns DATA_BODY (no bulk_export_class) ->
  # bulk_ok=FALSE -> falls through to live -> returns dataset.
  DATA_BODY <- '{"data":[
    {"date":"2023-01-01","period":"2023Q1","value":100.0},
    {"date":"2023-04-01","period":"2023Q2","value":101.5}
  ]}'
  with_mock_eolas(DATA_BODY, code = {
    df <- eolas_get("nz_cpi")
    expect_s3_class(df, "eolas_dataset")
    expect_equal(nrow(df), 2L)
    expect_equal(attr(df, "eolas_name"), "nz_cpi")
  })
})
