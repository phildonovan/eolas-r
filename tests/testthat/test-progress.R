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
