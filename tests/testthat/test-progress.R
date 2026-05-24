library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Progress bar behaviour — tests for .eolas_resolve_progress() and
# progress-kwarg plumbing in eolas_download_bulk / eolas_sync_bulk.
# ---------------------------------------------------------------------------

FAKE_PARQUET <- c(charToRaw("PAR1"), as.raw(rep(0L, 12)), charToRaw("PAR1"))

BULK_META_JSON <- jsonlite::toJSON(
  list(name = "nz_cpi", title = "NZ CPI", source = "Stats NZ",
       namespace = "statsnz", table = "nz_cpi"),
  auto_unbox = TRUE
)

# Build a fake httr2_response list.
# httr2 1.x resp_body_json() checks resp$cache (must be an environment).
fake_resp <- function(status = 200L, body = FAKE_PARQUET,
                      content_type = "application/octet-stream",
                      extra_headers = list()) {
  headers_list <- c(
    list(`content-type` = content_type,
         `content-length` = as.character(length(body))),
    extra_headers
  )
  structure(
    list(method = "GET", url = "https://api.eolas.fyi/test",
         status_code = status,
         headers = structure(headers_list, class = "httr2_headers"),
         body = body,
         cache = new.env(parent = emptyenv())),
    class = "httr2_response"
  )
}

fake_meta_resp <- function() {
  fake_resp(200L, charToRaw(BULK_META_JSON), "application/json")
}

# Set up the package-internal API key so all mocked calls succeed.
set_test_key <- function() {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)
}

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
  set_test_key()
  ns         <- getNamespace("eolas")
  call_count <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) fake_meta_resp()
      else fake_resp(200L, FAKE_PARQUET)
    },
    .env = ns
  )
  raw <- eolas_download_bulk("nz_cpi", path = NULL, progress = TRUE)
  expect_true(is.raw(raw))
  expect_gt(length(raw), 0L)
})
