# Pro trial notices + eolas_plan_error (eolas 1.14.0)

NOTICE <- "Pro trial: 50,000 rows/request, no bulk downloads; ends 2026-10-28. Large pulls are paginated and slow on trial - upgrade: https://eolas.nz/pricing"

test_that("X-Eolas-Notice warns once per session with class eolas_plan_notice", {
  .eolas_runtime$notices_seen <- NULL
  resp <- httr2::response(status_code = 200, headers = list("X-Eolas-Notice" = NOTICE))
  expect_warning(eolas_check_status(resp), class = "eolas_plan_notice")
  expect_no_warning(eolas_check_status(resp))
  .eolas_runtime$notices_seen <- NULL
})

test_that("no notice header, no warning", {
  resp <- httr2::response(status_code = 200)
  expect_no_warning(eolas_check_status(resp))
})

for (case in list(list(402L, "trial_expired"), list(403L, "bulk_not_in_plan"),
                  list(429L, "trial_soft_cap"))) {
  local({
    status <- case[[1]]
    code <- case[[2]]
    test_that(paste("plan refusal", code, "raises eolas_plan_error"), {
      resp <- httr2::response(
        status_code = status,
        headers = list("X-Eolas-Error-Code" = code, "Content-Type" = "application/json"),
        body = charToRaw('{"detail": "Not in your plan."}')
      )
      err <- tryCatch(eolas_check_status(resp), eolas_plan_error = function(e) e)
      expect_s3_class(err, "eolas_plan_error")
      expect_equal(err$code, code)
      expect_equal(err$status, status)
      expect_match(conditionMessage(err), "eolas.nz/pricing")
    })
  })
}

test_that("a plain 403 is still an authentication error, not a plan error", {
  resp <- httr2::response(
    status_code = 403, headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"detail": "API key is inactive."}')
  )
  err <- tryCatch(eolas_check_status(resp), error = function(e) e)
  expect_false(inherits(err, "eolas_plan_error"))
  expect_match(conditionMessage(err), "Authentication error")
})

test_that(".eolas_show_plan prints the plan notice from /v1/me", {
  old <- Sys.getenv("EOLAS_NO_NOTICE")
  Sys.unsetenv("EOLAS_NO_NOTICE")
  on.exit(Sys.setenv(EOLAS_NO_NOTICE = old), add = TRUE)
  me <- jsonlite::toJSON(list(plan = "trial", notice = NOTICE), auto_unbox = TRUE)
  with_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2::response(status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(me)))
    },
    .package = "eolas",
    expect_message(.eolas_show_plan("vs_trial"), "paginated")
  )
})

test_that(".eolas_show_plan is silent when the lookup fails", {
  old <- Sys.getenv("EOLAS_NO_NOTICE")
  Sys.unsetenv("EOLAS_NO_NOTICE")
  on.exit(Sys.setenv(EOLAS_NO_NOTICE = old), add = TRUE)
  with_mocked_bindings(
    eolas_http_perform = function(req) stop("offline"),
    .package = "eolas",
    expect_silent(.eolas_show_plan("vs_x"))
  )
})

test_that("EOLAS_NO_NOTICE skips the lookup", {
  called <- FALSE
  with_mocked_bindings(
    eolas_http_perform = function(req) { called <<- TRUE; stop("nope") },
    .package = "eolas",
    .eolas_show_plan("vs_x")
  )
  expect_false(called)
})

test_that("trial capped warning says slow + upgrade for bulk, not get_local", {
  resp <- httr2::response(status_code = 200, headers = list(
    "X-Eolas-Truncated" = "true", "X-Plan-Row-Cap" = "50000", "X-Plan" = "trial"))
  expect_warning(.eolas_warn_if_truncated("big", resp), "slow")
  expect_warning(.eolas_warn_if_truncated("big", resp), "bulk Parquet/CSV")
})

test_that("non-trial capped warning keeps the get_local hint", {
  resp <- httr2::response(status_code = 200, headers = list(
    "X-Eolas-Truncated" = "true", "X-Plan-Row-Cap" = "50000", "X-Plan" = "free"))
  expect_warning(.eolas_warn_if_truncated("big", resp), "whole table")
})

test_that("a body-less plan refusal (HEAD) uses the notice header, not the CF fallback", {
  resp <- httr2::response(status_code = 403, headers = list(
    "X-Eolas-Error-Code" = "bulk_not_in_plan", "X-Eolas-Notice" = NOTICE))
  err <- tryCatch(eolas_check_status(resp), eolas_plan_error = function(e) e)
  expect_s3_class(err, "eolas_plan_error")
  expect_match(conditionMessage(err), "no bulk downloads")
  expect_false(grepl("CF gateway", conditionMessage(err)))
})
