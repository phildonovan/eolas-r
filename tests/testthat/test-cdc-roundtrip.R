library(testthat)

# Round-trip invariant for the CDC OUT path: merge(baseline + feed) == full re-download.
# Mirrors the Python client's tests/test_cdc_roundtrip.py case-for-case — the two clients must
# reconstruct identical current state from the same feed.

# Canonicalise for frame comparison: sort rows by pk, sort columns by name, reset row names.
# Row/column order is an implementation detail; the invariant is set+content equality with bulk.
.canon <- function(df, pk) {
  df <- df[do.call(order, df[pk]), , drop = FALSE]
  df <- df[, order(names(df)), drop = FALSE]
  rownames(df) <- NULL
  df
}


test_that("append-only multi-column pk round-trip equals bulk", {
  pk <- c("week", "fuel", "variable", "unit")
  baseline <- data.frame(
    week = c("2026-W01", "2026-W01", "2026-W02", "2026-W02"),
    fuel = c("91", "diesel", "91", "diesel"),
    variable = rep("price", 4), unit = rep("nzd_per_l", 4),
    value = c(2.50, 2.10, 2.55, 2.12), stringsAsFactors = FALSE)
  changes <- data.frame(
    `_eolas_seq` = 1:2, `_eolas_op` = c("I", "I"),
    week = c("2026-W03", "2026-W03"), fuel = c("91", "diesel"),
    variable = c("price", "price"), unit = c("nzd_per_l", "nzd_per_l"),
    value = c(2.60, 2.15), check.names = FALSE, stringsAsFactors = FALSE)
  bulk_s1 <- rbind(baseline, data.frame(
    week = c("2026-W03", "2026-W03"), fuel = c("91", "diesel"),
    variable = c("price", "price"), unit = c("nzd_per_l", "nzd_per_l"),
    value = c(2.60, 2.15), stringsAsFactors = FALSE))

  merged <- eolas_merge_changes(baseline, changes, pk)

  expect_equal(.canon(merged, pk), .canon(bulk_s1, pk))
  expect_false(any(grepl("^_eolas_", names(merged))))  # meta cols stripped
})


test_that("update(D+I) + delete + insert round-trip equals bulk", {
  baseline <- data.frame(id = 1:5, val = c("a", "b", "c", "d_old", "e"), stringsAsFactors = FALSE)
  changes <- data.frame(
    `_eolas_seq` = 1:4, `_eolas_op` = c("D", "D", "I", "I"),
    id = c(3, 4, 4, 6), val = c("c", "d_old", "d_new", "f"),
    check.names = FALSE, stringsAsFactors = FALSE)
  bulk_s1 <- data.frame(id = c(1, 2, 4, 5, 6), val = c("a", "b", "d_new", "e", "f"),
                        stringsAsFactors = FALSE)

  merged <- eolas_merge_changes(baseline, changes, "id")

  expect_equal(.canon(merged, "id"), .canon(bulk_s1, "id"))
})


test_that("SCD2 current_state_filter round-trip equals bulk", {
  baseline <- data.frame(id = 1:3, name = c("p1", "p2_old", "p3"),
                         is_current = c(TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
  changes <- data.frame(
    `_eolas_seq` = 1:4, `_eolas_op` = c("D", "I", "D", "I"),
    id = c(2, 2, 3, 4), name = c("p2_old", "p2_new", "p3", "p4"),
    is_current = c(TRUE, TRUE, TRUE, TRUE), check.names = FALSE, stringsAsFactors = FALSE)
  bulk_s1 <- data.frame(id = c(1, 2, 4), name = c("p1", "p2_new", "p4"),
                        is_current = c(TRUE, TRUE, TRUE), stringsAsFactors = FALSE)

  merged <- eolas_merge_changes(baseline, changes, "id", current_state_filter = "is_current = true")

  expect_equal(.canon(merged, "id"), .canon(bulk_s1, "id"))
})


test_that("idempotent replay of the same feed is a no-op", {
  baseline <- data.frame(id = c(1, 2), val = c("a", "b"), stringsAsFactors = FALSE)
  changes <- data.frame(
    `_eolas_seq` = 1:3, `_eolas_op` = c("D", "I", "I"),
    id = c(2, 2, 3), val = c("b", "b_new", "c"), check.names = FALSE, stringsAsFactors = FALSE)

  once <- eolas_merge_changes(baseline, changes, "id")
  twice <- eolas_merge_changes(once, changes, "id")

  expect_equal(.canon(once, "id"), .canon(twice, "id"))
})


test_that("current_state_filter is type-tolerant and skips absent column", {
  # boolean filter against character is_current
  df_chr <- data.frame(id = 1:2, is_current = c("true", "false"), stringsAsFactors = FALSE)
  expect_equal(nrow(eolas_apply_current_state_filter(df_chr, "is_current = true")), 1L)
  # absent column -> unchanged (append-only tables have no is_current)
  df_no <- data.frame(id = 1:2, v = c("a", "b"), stringsAsFactors = FALSE)
  expect_equal(nrow(eolas_apply_current_state_filter(df_no, "is_current = true")), 2L)
})


test_that("multi-column pk key is NA-safe and does not collide", {
  df <- data.frame(a = c("x", "x", NA), b = c("1", "12", "1"), stringsAsFactors = FALSE)
  k <- eolas:::.eolas_pk_key(df, c("a", "b"))
  expect_equal(length(unique(k)), 3L)   # ("x","1") vs ("x","12") must not collide; NA distinct
})
