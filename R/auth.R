# Package-level environment to hold the API key
.eolas_env <- new.env(parent = emptyenv())

# OS-keyring constants. Must match the Python client so a key saved from one
# language is readable from the other.
.KEYRING_SERVICE  <- "eolas"
.KEYRING_USERNAME <- "api-key"

# Internal helper: read from the OS keyring without hard-requiring the keyring
# package. Returns "" (empty string) when:
#   - keyring is not installed
#   - no entry exists under service="eolas", username="api-key"
#   - the backend is locked / unavailable (headless CI / server)
# Never raises -- callers treat "" as "not found".
.keyring_get <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    return("")
  }
  tryCatch(
    {
      val <- keyring::key_get(.KEYRING_SERVICE, username = .KEYRING_USERNAME)
      if (is.null(val) || !nzchar(val)) "" else val
    },
    error = function(e) ""
  )
}

#' Set your eolas API key
#'
#' Stores the key for the duration of the R session. Alternatively, set the
#' `EOLAS_API_KEY` environment variable or use [eolas_key_save()] to persist
#' it to the OS keyring so you never need to call this again.
#'
#' @param key An API key from <https://eolas.fyi/signup>.
#' @return The key, invisibly.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key_here")
#' }
eolas_key <- function(key) {
  .eolas_env$key <- key
  invisible(key)
}

#' Save your eolas API key to the OS keyring
#'
#' Stores the key in the OS-native credential store (macOS Keychain, Windows
#' Credential Manager, Linux Secret Service) under
#' `service = "eolas"`, `username = "api-key"`. Once saved, [eolas_key()] and
#' every `eolas_get_*()` call will find the key automatically -- no environment
#' variable or explicit call needed in future sessions.
#'
#' The same keyring slot is read by the Python `eolas-data` client, so a key
#' saved from R is immediately available in Python and vice versa.
#'
#' Requires the `keyring` package. On Linux, `libsecret-1-dev` system headers
#' are needed before `install.packages("keyring")`.
#'
#' @param key The API key to save. `NULL` (default) prompts interactively via
#'   [askpass::askpass()] (if available) or [readline()].
#' @return Invisibly `NULL`.
#' @export
#' @seealso [eolas_key_clear()], [eolas_key_status()]
#' @examples
#' \dontrun{
#' eolas_key_save()          # interactive prompt
#' eolas_key_save("vs_...")  # non-interactive
#' }
eolas_key_save <- function(key = NULL) {
  rlang::check_installed(
    "keyring",
    reason = "to save your eolas API key to the OS keyring"
  )

  if (is.null(key)) {
    # Prefer askpass for masked input; fall back to readline.
    if (requireNamespace("askpass", quietly = TRUE)) {
      key <- askpass::askpass("Enter your eolas API key: ")
    } else {
      key <- readline("Enter your eolas API key: ")
    }
  }

  if (is.null(key) || !nzchar(key)) {
    cli::cli_alert_warning(c(
      "No key provided.",
      "i" = "You can also set {.envvar EOLAS_API_KEY} in {.path ~/.Renviron} instead."
    ))
    return(invisible(NULL))
  }

  keyring::key_set_with_value(
    service  = .KEYRING_SERVICE,
    username = .KEYRING_USERNAME,
    password = key
  )
  masked <- .mask_key(key)
  service <- .KEYRING_SERVICE
  cli::cli_alert_success(
    "Saved key {.field {masked}} to OS keyring (service {.val {service}})"
  )
  invisible(NULL)
}

#' Remove your eolas API key from the OS keyring
#'
#' Deletes the entry stored by [eolas_key_save()]. Does not affect the
#' `EOLAS_API_KEY` environment variable or the in-session key set by
#' [eolas_key()].
#'
#' @return Invisibly `NULL`.
#' @export
#' @seealso [eolas_key_save()], [eolas_key_status()]
#' @examples
#' \dontrun{
#' eolas_key_clear()
#' }
eolas_key_clear <- function() {
  rlang::check_installed(
    "keyring",
    reason = "to clear your eolas API key from the OS keyring"
  )
  tryCatch(
    {
      keyring::key_delete(.KEYRING_SERVICE, username = .KEYRING_USERNAME)
      cli::cli_alert_success("Cleared eolas API key from OS keyring.")
    },
    error = function(e) {
      cli::cli_alert_info("No eolas API key found in OS keyring (nothing to clear).")
    }
  )
  invisible(NULL)
}

#' Show which source is supplying your eolas API key
#'
#' Checks all sources in precedence order and reports the first one that has a
#' key, masking all but the first eight characters for safety.
#'
#' Precedence:
#' 1. In-session key set by [eolas_key()]
#' 2. `EOLAS_API_KEY` environment variable
#' 3. OS keyring (via the `keyring` package)
#' 4. `~/.eolas/config.json` (as written by the Python CLI `eolas auth set-key`)
#'
#' @return A character string describing the key source (invisibly). Primarily
#'   called for its side-effect of printing a status message.
#' @export
#' @seealso [eolas_key_save()], [eolas_key_clear()]
#' @examples
#' \dontrun{
#' eolas_key_status()
#' }
eolas_key_status <- function() {
  session_key <- .eolas_env$key
  if (!is.null(session_key) && nzchar(session_key)) {
    masked <- .mask_key(session_key)
    cli::cli_inform(c(
      "key:    {.field {masked}}",
      "source: in-session ({.fn eolas_key})"
    ))
    return(invisible("session"))
  }

  env_key <- Sys.getenv("EOLAS_API_KEY", unset = "")
  if (nzchar(env_key)) {
    masked <- .mask_key(env_key)
    cli::cli_inform(c(
      "key:    {.field {masked}}",
      "source: env {.envvar EOLAS_API_KEY}"
    ))
    return(invisible("env"))
  }

  kr_key <- .keyring_get()
  if (nzchar(kr_key)) {
    masked <- .mask_key(kr_key)
    service <- .KEYRING_SERVICE
    cli::cli_inform(c(
      "key:    {.field {masked}}",
      "source: OS keyring (service {.val {service}})"
    ))
    return(invisible("keyring"))
  }

  cfg_key <- .config_file_get_key()
  if (nzchar(cfg_key)) {
    masked <- .mask_key(cfg_key)
    cli::cli_inform(c(
      "key:    {.field {masked}}",
      "source: config file ({.path ~/.eolas/config.json})"
    ))
    return(invisible("config"))
  }

  cli::cli_alert_warning("No API key configured.")
  cli::cli_inform(c(
    "Options:",
    "*" = "{.run eolas::eolas_key_save()} -- OS keyring (recommended for workstations)",
    "*" = "{.run [eolas_key()](eolas::eolas_key())} -- in-session only",
    "*" = "{.code Sys.setenv(EOLAS_API_KEY = \"vs_...\")} -- environment variable"
  ))
  invisible("none")
}

# Internal: mask all but the first 8 characters of a key.
.mask_key <- function(key) {
  if (!nzchar(key)) return("(none)")
  paste0(substr(key, 1, 8), "...")
}

# Internal: read api_key from ~/.eolas/config.json (the slot the Python CLI's
# `eolas auth set-key` writes). Returns "" when absent/unreadable. The OS
# keyring is the preferred cross-language slot; this is a fallback so a key set
# only via the Python CLI is still found in R.
.config_file_get_key <- function() {
  cfg_path <- .eolas_config_file()
  if (!file.exists(cfg_path)) return("")
  tryCatch(
    {
      cfg <- jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = TRUE)
      val <- cfg[["api_key"]] %||% ""
      if (is.null(val) || !nzchar(val)) "" else as.character(val)
    },
    error = function(e) ""
  )
}

eolas_get_key_internal <- function() {
  # Precedence: in-session -> EOLAS_API_KEY -> OS keyring -> ~/.eolas/config.json -> error
  key <- .eolas_env$key %||%
         Sys.getenv("EOLAS_API_KEY", unset = "") %or_empty%
         .keyring_get() %or_empty%
         .config_file_get_key()
  if (is.null(key) || !nzchar(key)) {
    stop(
      "No API key found. ",
      "Call eolas_key_save() to store it in the OS keyring, ",
      "or set the EOLAS_API_KEY environment variable. ",
      "Get a free key at https://eolas.fyi/signup",
      call. = FALSE
    )
  }
  key
}

# Like %||% but treats "" as NULL (so falsy empty strings fall through).
`%or_empty%` <- function(x, y) if (!is.null(x) && nzchar(x)) x else y

`%||%` <- function(x, y) if (!is.null(x)) x else y
