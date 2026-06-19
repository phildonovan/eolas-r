library(testthat)

# Progress bar behaviour — .eolas_resolve_progress() and download_bulk plumbing.
# Shared constants + set_test_key() live in helper.R.

# ---------------------------------------------------------------------------
# .eolas_resolve_progress — unit tests
#
# with_mocked_bindings(code, ..., .package=) is the correct testthat 3.x API:
#   - first arg is the code block (unevaluated)
#   - named args are the mock bindings
# ---------------------------------------------------------------------------

test_that("explicit TRUE wins over EOLAS_NO_PROGRESS and interactive()=FALSE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = "1"), {
    with_mocked_bindings(
      {
        expect_true(eolas:::.eolas_resolve_progress(TRUE))
      },
      .eolas_is_interactive = function() FALSE,
      .package = "eolas"
    )
  })
})

test_that("explicit FALSE wins over interactive()=TRUE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = ""), {
    with_mocked_bindings(
      {
        expect_false(eolas:::.eolas_resolve_progress(FALSE))
      },
      .eolas_is_interactive = function() TRUE,
      .package = "eolas"
    )
  })
})

test_that("EOLAS_NO_PROGRESS=1 suppresses when interactive()=TRUE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = "1"), {
    with_mocked_bindings(
      {
        expect_false(eolas:::.eolas_resolve_progress(NULL))
      },
      .eolas_is_interactive = function() TRUE,
      .package = "eolas"
    )
  })
})

test_that("NULL with interactive()=TRUE returns TRUE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = ""), {
    with_mocked_bindings(
      {
        expect_true(eolas:::.eolas_resolve_progress(NULL))
      },
      .eolas_is_interactive = function() TRUE,
      .package = "eolas"
    )
  })
})

test_that("NULL with interactive()=FALSE returns FALSE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = ""), {
    with_mocked_bindings(
      {
        expect_false(eolas:::.eolas_resolve_progress(NULL))
      },
      .eolas_is_interactive = function() FALSE,
      .package = "eolas"
    )
  })
})

test_that("EOLAS_NO_PROGRESS=0 does not suppress when interactive()=TRUE", {
  set_test_key()
  withr::with_envvar(list(EOLAS_NO_PROGRESS = "0"), {
    with_mocked_bindings(
      {
        expect_true(eolas:::.eolas_resolve_progress(NULL))
      },
      .eolas_is_interactive = function() TRUE,
      .package = "eolas"
    )
  })
})

test_that(".eolas_dataset_field avoids tibble $ warning for absent table column", {
  meta <- tibble::tibble(name = "nz_addresses", namespace = "linz")
  expect_silent({
    table <- eolas:::.eolas_dataset_field(
      meta, "table",
      eolas:::.eolas_dataset_field(meta, "name", "fallback")
    )
  })
  expect_equal(table, "nz_addresses")
})

test_that("progress phase selectors split download and read", {
  phases <- eolas:::.eolas_resolve_progress_phases("download")
  expect_true(phases$download)
  expect_false(phases$read)

  phases <- eolas:::.eolas_resolve_progress_phases("read")
  expect_false(phases$download)
  expect_true(phases$read)

  phases <- eolas:::.eolas_resolve_progress_phases("both")
  expect_true(phases$download)
  expect_true(phases$read)

  phases <- eolas:::.eolas_resolve_progress_phases("none")
  expect_false(phases$download)
  expect_false(phases$read)
})

test_that(".eolas_resolve_progress routes phases correctly", {
  expect_true(eolas:::.eolas_resolve_progress(TRUE, "download"))
  expect_true(eolas:::.eolas_resolve_progress("read", "read"))
  expect_false(eolas:::.eolas_resolve_progress("download", "read"))
})

# ---------------------------------------------------------------------------
# eolas_download_bulk: bytes mode (path=NULL) ignores progress arg
# ---------------------------------------------------------------------------

test_that("eolas_download_bulk bytes mode (path=NULL) succeeds with progress=TRUE", {
  with_mock_bulk(FAKE_PARQUET, code = {
    raw <- eolas_download_bulk("nz_cpi", path = NULL, progress = TRUE)
    expect_true(is.raw(raw))
    expect_gt(length(raw), 0L)
  })
})
