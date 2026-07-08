# Polish (2026-07-08): a transport failure on the arrow attempt in
# .eolas_fetch_df is re-raised immediately, not swallowed-and-retried as JSON
# (which paid the req_timeout budget twice -> ~2x latency on an unreachable host).

test_that(".eolas_fetch_df re-raises a transport failure without a JSON retry", {
  skip_if_not_installed("arrow")
  # NULL = "arrow support unknown", so the arrow attempt actually runs.
  withr::defer(assign("arrow_supported", NULL, envir = .eolas_runtime))
  assign("arrow_supported", NULL, envir = .eolas_runtime)

  calls <- 0L
  transport_err <- structure(
    class = c("httr2_failure", "httr2_error", "rlang_error", "error", "condition"),
    list(message = "Timeout was reached", call = NULL)
  )
  local_mocked_bindings(
    eolas_http_get = function(...) {
      calls <<- calls + 1L
      stop(transport_err)
    }
  )

  expect_error(
    .eolas_fetch_df("worksafe_fatalities", list(limit = 1L), "http://10.255.255.1:1"),
    class = "httr2_failure"
  )
  # Exactly ONE call: the arrow attempt re-raised immediately, with no
  # fall-through JSON retry that would pay the timeout budget a second time.
  expect_identical(calls, 1L)
})

test_that(".eolas_fetch_df falls through to JSON on a non-transport error", {
  skip_if_not_installed("arrow")
  withr::defer(assign("arrow_supported", NULL, envir = .eolas_runtime))
  assign("arrow_supported", NULL, envir = .eolas_runtime)

  calls <- 0L
  # A plain (non-httr2_failure) error on the arrow attempt must NOT short-circuit:
  # the JSON attempt still runs (second call), preserving the old-server fallback.
  local_mocked_bindings(
    eolas_http_get = function(...) {
      calls <<- calls + 1L
      stop("boom")
    }
  )
  expect_error(
    .eolas_fetch_df("worksafe_fatalities", list(limit = 1L), "http://10.255.255.1:1")
  )
  expect_identical(calls, 2L)
})
