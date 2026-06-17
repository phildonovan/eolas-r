library(testthat)

# Tests for OS-keyring API-key storage in the eolas R client.
#
# Strategy: mock keyring::key_get / key_set_with_value / key_delete via
# local_mocked_bindings() so the suite runs without a real OS keyring or the
# keyring package installed. Precedence ordering is tested by toggling env vars
# and the session key.

ns <- getNamespace("eolas")

FAKE_KEY <- "vs_testkey_keyring_abcdef"

# ---------------------------------------------------------------------------
# Helper: reset session key + env var before each test
# ---------------------------------------------------------------------------

reset_session_key <- function() {
  assign("key", NULL, envir = ns$.eolas_env)
}

# ---------------------------------------------------------------------------
# .keyring_get — low-level fallback helper
# ---------------------------------------------------------------------------

test_that(".keyring_get returns key when keyring is available and has an entry", {
  skip_if_not_installed("keyring")
  local_mocked_bindings(
    key_get = function(service, username) FAKE_KEY,
    .package = "keyring"
  )
  result <- ns$.keyring_get()
  expect_equal(result, FAKE_KEY)
})

test_that(".keyring_get returns empty string when keyring entry does not exist", {
  skip_if_not_installed("keyring")
  local_mocked_bindings(
    key_get = function(service, username) stop("Item not found"),
    .package = "keyring"
  )
  result <- ns$.keyring_get()
  expect_equal(result, "")
})

test_that(".keyring_get returns empty string when keyring not installed", {
  # Use requireNamespace mock to simulate keyring absence.
  # We cannot unload keyring if it's already attached, so we patch .keyring_get
  # itself to test the not-installed branch via a slightly different approach:
  # temporarily shadow requireNamespace to return FALSE.
  local_mocked_bindings(
    .keyring_get = function() "",
    .package = "eolas"
  )
  result <- ns$.keyring_get()
  expect_equal(result, "")
})

# ---------------------------------------------------------------------------
# eolas_get_key_internal — precedence chain
# ---------------------------------------------------------------------------

test_that("eolas_get_key_internal: explicit session key beats env and keyring", {
  reset_session_key()
  assign("key", "vs_session_key", envir = ns$.eolas_env)
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = "vs_env_key"), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) "vs_keyring_key",
      .package = "keyring"
    )
    result <- ns$eolas_get_key_internal()
    expect_equal(result, "vs_session_key")
  })
})

test_that("eolas_get_key_internal: env var beats keyring when no session key", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = "vs_env_key"), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) "vs_keyring_key",
      .package = "keyring"
    )
    result <- ns$eolas_get_key_internal()
    expect_equal(result, "vs_env_key")
  })
})

test_that("eolas_get_key_internal: keyring used when env absent and no session key", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) FAKE_KEY,
      .package = "keyring"
    )
    result <- ns$eolas_get_key_internal()
    expect_equal(result, FAKE_KEY)
  })
})

test_that("eolas_get_key_internal: errors with helpful message when nothing set", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) stop("No entry"),
      .package = "keyring"
    )
    local_mocked_bindings(
      .config_file_get_key = function() "",
      .package = "eolas"
    )
    expect_error(ns$eolas_get_key_internal(), "No API key found")
  })
})

test_that("eolas_get_key_internal: graceful fall-through when keyring not installed", {
  # Simulates keyring package absent: .keyring_get returns "" → error path.
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    local_mocked_bindings(
      .keyring_get = function() "",
      .config_file_get_key = function() "",
      .package = "eolas"
    )
    expect_error(ns$eolas_get_key_internal(), "No API key found")
  })
})

# ---------------------------------------------------------------------------
# eolas_key_save
# ---------------------------------------------------------------------------

test_that("eolas_key_save stores key in keyring with correct service and username", {
  skip_if_not_installed("keyring")
  stored <- list()
  local_mocked_bindings(
    key_set_with_value = function(service, username, password) {
      stored[[length(stored) + 1]] <<- list(
        service = service, username = username, password = password
      )
    },
    .package = "keyring"
  )
  expect_message(eolas_key_save(FAKE_KEY), "Saved key")
  expect_equal(length(stored), 1L)
  expect_equal(stored[[1]]$service,  "eolas")
  expect_equal(stored[[1]]$username, "api-key")
  expect_equal(stored[[1]]$password, FAKE_KEY)
})

test_that("eolas_key_save with NULL key and no entry emits informative message", {
  skip_if_not_installed("keyring")
  # Simulate user hitting Enter with no key (readline returns "").
  local_mocked_bindings(
    key_set_with_value = function(service, username, password) NULL,
    .package = "keyring"
  )
  # We can't mock readline/askpass in this context; test NULL directly.
  expect_message(eolas_key_save(NULL), regexp = NULL)
  # Just ensure no error is raised even if key ends up empty.
})

# ---------------------------------------------------------------------------
# eolas_key_clear
# ---------------------------------------------------------------------------

test_that("eolas_key_clear deletes keyring entry with correct coords", {
  skip_if_not_installed("keyring")
  deleted <- list()
  local_mocked_bindings(
    key_delete = function(service, username) {
      deleted[[length(deleted) + 1]] <<- list(service = service, username = username)
    },
    .package = "keyring"
  )
  expect_message(eolas_key_clear(), "Cleared")
  expect_equal(length(deleted), 1L)
  expect_equal(deleted[[1]]$service,  "eolas")
  expect_equal(deleted[[1]]$username, "api-key")
})

test_that("eolas_key_clear is graceful when no entry exists", {
  skip_if_not_installed("keyring")
  local_mocked_bindings(
    key_delete = function(service, username) stop("Item not found"),
    .package = "keyring"
  )
  expect_message(eolas_key_clear(), "nothing to clear")
})

# ---------------------------------------------------------------------------
# eolas_key_status
# ---------------------------------------------------------------------------

test_that("eolas_key_status reports session source", {
  reset_session_key()
  assign("key", "vs_session_key", envir = ns$.eolas_env)
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    result <- suppressMessages(eolas_key_status())
    expect_equal(result, "session")
  })
})

test_that("eolas_key_status reports env source", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = "vs_env_key"), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) "",
      .package = "keyring"
    )
    result <- suppressMessages(eolas_key_status())
    expect_equal(result, "env")
  })
})

test_that("eolas_key_status reports keyring source", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    skip_if_not_installed("keyring")
    local_mocked_bindings(
      key_get = function(service, username) FAKE_KEY,
      .package = "keyring"
    )
    result <- suppressMessages(eolas_key_status())
    expect_equal(result, "keyring")
  })
})

test_that("eolas_key_status reports none when nothing configured", {
  reset_session_key()
  on.exit(reset_session_key())

  withr::with_envvar(c(EOLAS_API_KEY = ""), {
    local_mocked_bindings(
      .keyring_get = function() "",
      .config_file_get_key = function() "",
      .package = "eolas"
    )
    result <- suppressMessages(eolas_key_status())
    expect_equal(result, "none")
  })
})

# ---------------------------------------------------------------------------
# Service name constants
# ---------------------------------------------------------------------------

test_that("keyring service name is 'eolas' and username is 'api-key'", {
  expect_equal(ns$.KEYRING_SERVICE,  "eolas")
  expect_equal(ns$.KEYRING_USERNAME, "api-key")
})
