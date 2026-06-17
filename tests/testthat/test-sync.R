library(testthat)

# eolas_sync() dispatcher — routes on cdc_serving_tier (mirrors Python TestSyncDispatcher).

CHANGELOG_META_JSON <- jsonlite::toJSON(
  list(
    name                 = "pharmac_schedule_history",
    namespace            = "pharmac",
    table                = "pharmac_schedule_history",
    cdc_serving_tier     = "changelog",
    pk_columns           = list("pharmacode", "time_frame"),
    current_state_filter = NULL
  ),
  auto_unbox = TRUE
)

SNAPSHOT_META_JSON <- jsonlite::toJSON(
  list(
    name             = "nz_cpi",
    namespace        = "statsnz",
    table            = "nz_cpi",
    cdc_serving_tier = "snapshot"
  ),
  auto_unbox = TRUE
)

test_that("eolas_sync dispatches snapshot-tier datasets to eolas_sync_bulk", {
  dispatched <- NULL
  set_test_key()
  with_mocked_bindings(
    {
      out <- tempfile(fileext = ".parquet")
      result <- eolas_sync("nz_cpi", path = out)
      expect_equal(dispatched, "bulk")
      expect_equal(result$status, "downloaded")
      expect_equal(result$current_snapshot_id, SNAPSHOT_V1)
    },
    eolas_info = function(name, base_url = NULL) {
      jsonlite::fromJSON(SNAPSHOT_META_JSON, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(name, path, ...) {
      dispatched <<- "bulk"
      list(
        status               = "downloaded",
        previous_snapshot_id = NA_character_,
        current_snapshot_id  = SNAPSHOT_V1,
        path                 = path,
        bytes_downloaded     = 1024L
      )
    },
    eolas_sync_changes = function(name, path, ...) {
      dispatched <<- "changes"
      list(status = "downloaded", sync_mode = "changelog")
    },
    .package = "eolas"
  )
})

test_that("eolas_sync dispatches changelog-tier datasets to eolas_sync_changes", {
  dispatched <- NULL
  set_test_key()
  with_mocked_bindings(
    {
      out <- tempfile(fileext = ".parquet")
      result <- eolas_sync("pharmac_schedule_history", path = out)
      expect_equal(dispatched, "changes")
      expect_equal(result$sync_mode, "changelog")
      expect_equal(result$current_seq, 514000L)
    },
    eolas_info = function(name, base_url = NULL) {
      jsonlite::fromJSON(CHANGELOG_META_JSON, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(name, path, ...) {
      dispatched <<- "bulk"
      list(status = "downloaded")
    },
    eolas_sync_changes = function(name, path, ...) {
      dispatched <<- "changes"
      list(
        status    = "downloaded",
        sync_mode = "changelog",
        current_seq = 514000L
      )
    },
    .package = "eolas"
  )
})

test_that("eolas_sync defaults to snapshot when cdc_serving_tier is absent", {
  dispatched <- NULL
  set_test_key()
  meta_no_tier <- jsonlite::toJSON(
    list(name = "old_dataset", namespace = "statsnz", table = "old_dataset"),
    auto_unbox = TRUE
  )
  with_mocked_bindings(
    {
      out <- tempfile(fileext = ".parquet")
      eolas_sync("old_dataset", path = out)
      expect_equal(dispatched, "bulk")
    },
    eolas_info = function(name, base_url = NULL) {
      jsonlite::fromJSON(meta_no_tier, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(name, path, ...) {
      dispatched <<- "bulk"
      list(status = "downloaded")
    },
    eolas_sync_changes = function(name, path, ...) {
      dispatched <<- "changes"
      list(status = "downloaded", sync_mode = "changelog")
    },
    .package = "eolas"
  )
})