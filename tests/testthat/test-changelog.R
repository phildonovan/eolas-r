library(testthat)

# Orchestration tests for eolas_sync_changes: cold-start baseline + watermark anchor, incremental
# paging + merge + watermark advance, unchanged no-op, and 410 self-heal. The HTTP/paging helpers
# are mocked; the REAL merge (cdc.R), atomic parquet write, and v2 sidecar are exercised. Mirrors
# the Python client's tests/test_sync_changes.py integration cases.

.meta <- list(name = "t", namespace = "linz", table = "t",
              cdc_serving_tier = "changelog", pk_columns = list("id"),
              current_state_filter = "is_current = true", current_snapshot_id = "snapA")

.changes_df <- function(seq, op, id, name, is_current = TRUE) {
  data.frame(`_eolas_seq` = seq, `_eolas_op` = op, id = id, name = name,
             is_current = is_current, check.names = FALSE, stringsAsFactors = FALSE)
}

.read_sc <- function(path) jsonlite::fromJSON(readLines(paste0(path, ".eolas-meta.json"), warn = FALSE),
                                              simplifyVector = TRUE)


test_that("cold start: baselines via bulk + anchors watermark + writes v2 sidecar", {
  out <- tempfile(fileext = ".parquet")
  local_mocked_bindings(
    eolas_info = function(name, ...) .meta,
    eolas_sync_bulk = function(name, path, ...) {
      arrow::write_parquet(data.frame(id = 1:2, name = c("p1", "p2"), is_current = c(TRUE, TRUE)), path)
      list(status = "downloaded", current_snapshot_id = "snapA")
    },
    .eolas_fetch_seq_high = function(name, ...) 100,
    .package = "eolas"
  )
  r <- eolas_sync_changes("t", path = out)
  expect_equal(r$status, "downloaded")
  expect_equal(r$current_seq, 100)
  expect_equal(r$ops_applied, 0L)
  sc <- .read_sc(out)
  expect_equal(sc$schema_version, 2L)
  expect_equal(sc$sync_mode, "changelog")
  expect_equal(sc$watermark_seq, 100)
})


test_that("incremental: pages changes, merges into local file, advances watermark", {
  out <- tempfile(fileext = ".parquet")
  arrow::write_parquet(
    data.frame(id = 1:2, name = c("p1", "p2_old"), is_current = c(TRUE, TRUE)), out)
  .eolas_write_changelog_sidecar(paste0(out, ".eolas-meta.json"), "t", "parquet",
                                 list("id"), "is_current = true", "snapA", 100)
  changes <- rbind(
    .changes_df(101, "D", 2, "p2_old"),   # update id 2: expire old ...
    .changes_df(102, "I", 2, "p2_new"),   # ... insert new
    .changes_df(103, "I", 3, "p3"))       # insert id 3
  local_mocked_bindings(
    eolas_info = function(name, ...) .meta,
    .eolas_fetch_all_change_pages = function(name, since_seq, ...) list(changes = changes, final_seq = 103),
    .package = "eolas"
  )
  r <- eolas_sync_changes("t", path = out)
  expect_equal(r$status, "updated")
  expect_equal(r$current_seq, 103)
  expect_equal(r$ops_applied, 3L)
  merged <- as.data.frame(arrow::read_parquet(out))
  merged <- merged[order(merged$id), ]
  expect_equal(merged$id, c(1, 2, 3))
  expect_equal(merged$name, c("p1", "p2_new", "p3"))   # id 2 updated, id 3 inserted, id 1 kept
  expect_equal(.read_sc(out)$watermark_seq, 103)        # watermark advanced
})


test_that("unchanged: empty feed -> no rewrite, watermark held", {
  out <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(id = 1, name = "p1", is_current = TRUE), out)
  .eolas_write_changelog_sidecar(paste0(out, ".eolas-meta.json"), "t", "parquet",
                                 list("id"), "is_current = true", "snapA", 105)
  before_mtime <- file.mtime(out)
  local_mocked_bindings(
    eolas_info = function(name, ...) .meta,
    .eolas_fetch_all_change_pages = function(name, since_seq, ...) list(changes = data.frame(), final_seq = 105),
    .package = "eolas"
  )
  r <- eolas_sync_changes("t", path = out)
  expect_equal(r$status, "unchanged")
  expect_equal(r$ops_applied, 0L)
  expect_equal(r$current_seq, 105)
  expect_equal(file.mtime(out), before_mtime)   # data file untouched
})


test_that("410 watermark expired: self-heals by re-baselining", {
  out <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(id = 1, name = "p1", is_current = TRUE), out)
  .eolas_write_changelog_sidecar(paste0(out, ".eolas-meta.json"), "t", "parquet",
                                 list("id"), "is_current = true", "snapOld", 5)
  local_mocked_bindings(
    eolas_info = function(name, ...) .meta,
    .eolas_fetch_all_change_pages = function(name, since_seq, ...)
      cli::cli_abort("expired", class = "eolas_watermark_expired"),
    eolas_sync_bulk = function(name, path, ...) {
      arrow::write_parquet(data.frame(id = 1:3, name = c("p1", "p2", "p3"),
                                      is_current = c(TRUE, TRUE, TRUE)), path)
      list(status = "downloaded", current_snapshot_id = "snapNew")
    },
    .eolas_fetch_seq_high = function(name, ...) 500,
    .package = "eolas"
  )
  r <- eolas_sync_changes("t", path = out)
  expect_equal(r$status, "updated")
  expect_equal(r$current_seq, 500)              # re-anchored at the new feed head
  expect_equal(r$ops_applied, 0L)
  expect_equal(.read_sc(out)$watermark_seq, 500)
})
