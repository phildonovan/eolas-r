# Polish (2026-07-08): EOLAS_BASE_URL env var is now honoured (previously the
# base URL was a hardcoded literal, so the env var was silently inert).

test_that(".eolas_default_base_url honours the EOLAS_BASE_URL env var", {
  withr::local_envvar(EOLAS_BASE_URL = "https://api-staging.eolas.test")
  expect_equal(.eolas_default_base_url(), "https://api-staging.eolas.test")
})

test_that(".eolas_default_base_url falls back to prod when the env is unset", {
  withr::local_envvar(EOLAS_BASE_URL = NA)
  expect_equal(.eolas_default_base_url(), "https://api.eolas.fyi")
})
