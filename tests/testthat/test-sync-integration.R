library(testthat)

# ---------------------------------------------------------------------------
# Integration tests: eolas_sync() + eolas_compact() against prod API
# ---------------------------------------------------------------------------
# These tests hit api.eolas.fyi and require a valid API key.
# They are skipped in CI (no key) and in CRAN checks.
#
# To run manually:
#   Sys.setenv(EOLAS_API_KEY = "vs_5QZ-...")
#   testthat::test_file("tests/testthat/test-sync-integration.R")
# ---------------------------------------------------------------------------

skip_if_not(
  nzchar(Sys.getenv("EOLAS_API_KEY")),
  message = "EOLAS_API_KEY not set — skipping integration tests"
)
skip_if_not_installed("arrow")
skip_on_cran()
# Skip when running alongside unit tests (mock leakage risk from the
# stateful eolas_http_perform mocks in test-get-library.R).
# Run this file in isolation: devtools::test(filter = "sync-integration")
skip_if(
  isTRUE(as.logical(Sys.getenv("EOLAS_SKIP_INTEGRATION", "false"))),
  message = "EOLAS_SKIP_INTEGRATION=true — skipping"
)
# Detect if any mock is currently active by checking the binding.
# If eolas_http_perform has been replaced (its body won't match the
# original req_perform call), skip to avoid mock-pollution.
.check_mock_clean <- function() {
  ns <- getNamespace("eolas")
  fn <- get("eolas_http_perform", envir = ns)
  body_str <- paste(deparse(body(fn)), collapse = "")
  # The real implementation contains "req_perform"; a mock will not.
  grepl("req_perform", body_str, fixed = TRUE)
}
skip_if(
  !.check_mock_clean(),
  message = "eolas_http_perform appears to be mocked — skipping integration tests"
)

# Ensure the session uses the real API key, not a test fake left by unit tests.
{
  ns  <- getNamespace("eolas")
  key <- Sys.getenv("EOLAS_API_KEY", unset = "")
  if (nzchar(key)) assign("key", key, envir = ns$.eolas_env)
}

# Use a fresh temp dir for every test file.
LOCAL_LIB <- withr::local_tempdir(.local_envir = parent.env(environment()))

# ---------------------------------------------------------------------------

test_that("eolas_sync: first sync of doc_huts returns snapshot_full", {
  r <- eolas_sync("doc_huts", library_dir = LOCAL_LIB, progress = FALSE)

  expect_s3_class(r, "eolas_sync_result")
  expect_equal(r$status, "snapshot_full")
  expect_equal(r$dataset, "doc_huts")
  expect_gt(r$bytes_downloaded, 0L)
  expect_gte(r$rows_added, 1L)
  expect_equal(r$files_added, 1L)
  expect_null(r$error)

  # Manifest written
  expect_true(
    file.exists(file.path(LOCAL_LIB, "doc_huts", "_eolas-manifest.json"))
  )
})

test_that("eolas_sync: second sync returns unchanged (same snapshot)", {
  # Depends on the first test having already synced doc_huts into LOCAL_LIB.
  r <- eolas_sync("doc_huts", library_dir = LOCAL_LIB, progress = FALSE)

  expect_equal(r$status, "unchanged")
  expect_equal(r$bytes_downloaded, 0L)
  expect_equal(r$rows_added, 0L)
})

test_that("eolas_get_local: reads synced doc_huts from library", {
  withr::with_envvar(list(EOLAS_LIBRARY = LOCAL_LIB), {
    df <- eolas_get_local("doc_huts", progress = FALSE)
    expect_s3_class(df, "data.frame")
    expect_gte(nrow(df), 1L)
    cat("  from library:", nrow(df), "rows\n")
  })
})

test_that("eolas_compact: compacts doc_huts dir (no-op when single file)", {
  ddir <- file.path(LOCAL_LIB, "doc_huts")
  r <- eolas_compact(dataset_dir = ddir)

  expect_s3_class(r, "eolas_compact_result")
  # single file → no-op
  expect_equal(r$files_after, 1L)
})

test_that("eolas_sync_all: syncs c('doc_huts') returns named result list", {
  td2 <- withr::local_tempdir()
  results <- eolas_sync_all(
    library_dir    = td2,
    datasets       = "doc_huts",
    max_concurrent = 1L,
    progress       = FALSE
  )
  expect_equal(length(results), 1L)
  expect_equal(results[["doc_huts"]]$status, "snapshot_full")
})

test_that("eolas_sync_all: auto-discover syncs previously synced datasets", {
  # LOCAL_LIB already has doc_huts from the first test.
  results <- eolas_sync_all(library_dir = LOCAL_LIB, max_concurrent = 1L,
                             progress = FALSE)
  expect_gte(length(results), 1L)
  expect_true("doc_huts" %in% names(results))
  # Second sync should be unchanged
  expect_equal(results[["doc_huts"]]$status, "unchanged")
})
