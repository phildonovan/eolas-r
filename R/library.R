# Library-directory resolution for the eolas R client.
#
# Implements the same precedence chain as the Python eolas-data client:
#
#   1. Explicit cache_dir= argument to eolas_get_local()  (handled by callers)
#   2. EOLAS_LIBRARY environment variable
#   3. library_dir in ~/.eolas/config.json
#   4. Interactive prompt (interactive sessions only, once per session)
#   5. ~/.cache/eolas/  (silent fallback with one-time nudge)
#
# The config file path mirrors the Python client so that a library set from
# Python (`eolas library set`) is immediately honoured in R and vice versa.
#
# Interactive gating uses R's built-in `interactive()` — the standard,
# cross-platform, stdlib function used by usethis, askpass, gitcreds,
# keyring, and every other package that needs to gate on a live user session.
# It is the direct R equivalent of Python's `sys.stdin.isatty()`.

# Path to the shared config file (same as Python's ~/.eolas/config.json).
.eolas_config_file <- function() {
  file.path(path.expand("~"), ".eolas", "config.json")
}

# Per-session flags stored in a package-namespace environment (not options(),
# which leaks into user globals). Pattern from tibble's one-time message gate.
.eolas_lib_runtime <- new.env(parent = emptyenv())
.eolas_lib_runtime$headless_info_emitted <- FALSE
.eolas_lib_runtime$prompt_fired          <- FALSE

# Thin wrapper around base::interactive() so tests can mock it via
# local_mocked_bindings(.package = "eolas").  The base binding is in the
# base namespace and cannot be patched at the call site; wrapping it here
# gives the test suite a seam to override.
.eolas_is_interactive <- function() interactive()


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.eolas_read_library_dir_from_config <- function() {
  cfg_path <- .eolas_config_file()
  if (!file.exists(cfg_path)) return("")
  tryCatch(
    {
      cfg <- jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = TRUE)
      val <- cfg[["library_dir"]] %||% ""
      if (is.null(val) || !nzchar(val)) "" else as.character(val)
    },
    error = function(e) ""
  )
}

.eolas_write_config_key <- function(key, value) {
  cfg_path <- .eolas_config_file()
  cfg_dir  <- dirname(cfg_path)
  if (!dir.exists(cfg_dir)) {
    dir.create(cfg_dir, recursive = TRUE, mode = "0700", showWarnings = FALSE)
  }

  cfg <- list()
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(
      jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = FALSE),
      error = function(e) list()
    )
  }
  cfg[[key]] <- value
  writeLines(
    jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE),
    cfg_path
  )
  Sys.chmod(cfg_path, mode = "0600")
}

.eolas_remove_config_key <- function(key) {
  cfg_path <- .eolas_config_file()
  if (!file.exists(cfg_path)) return(invisible(NULL))
  tryCatch(
    {
      cfg <- jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = FALSE)
      cfg[[key]] <- NULL
      writeLines(
        jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE),
        cfg_path
      )
      Sys.chmod(cfg_path, mode = "0600")
    },
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

.eolas_emit_headless_info_once <- function() {
  if (isTRUE(.eolas_lib_runtime$headless_info_emitted)) return(invisible())
  .eolas_lib_runtime$headless_info_emitted <- TRUE
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_inform(c(
      "i" = "eolas: using {.path ~/.cache/eolas/} (transient OS cache).",
      " " = "For persistent storage, set {.envvar EOLAS_LIBRARY}=/path/to/lib",
      " " = "or run interactively and the package will prompt you."
    ))
  } else {
    cli::cli_alert_info(c(
      "Using {.path ~/.cache/eolas/} (transient OS cache).",
      "i" = "For persistent storage, set {.envvar EOLAS_LIBRARY}{.code =/path/to/lib}",
      "i" = "or run interactively and the package will prompt you."
    ))
  }
}

# Thin wrapper around utils::menu() so tests can mock it via
# with_mocked_bindings(.package = "eolas").
.eolas_cli_select <- function(choices, title) {
  utils::menu(choices = choices, title = title)
}

# Session-once interactive prompt for library directory selection.
# Called when: interactive() is TRUE AND no env/config value is set AND
# the prompt has not already fired this session.
.eolas_prompt_library_dir <- function() {
  if (isTRUE(.eolas_lib_runtime$prompt_fired)) return(NULL)
  .eolas_lib_runtime$prompt_fired <- TRUE

  choice <- .eolas_cli_select(
    choices = c(
      "~/eolas-library  (user-wide, persistent — recommended)",
      "./eolas-library  (this project)",
      "Custom path...",
      "Stay with ~/.cache/eolas  (don't ask again)"
    ),
    title = "eolas: no library configured. Where should data files live?"
  )

  resolved <- switch(as.character(choice),
    "1" = path.expand("~/eolas-library"),
    "2" = file.path(getwd(), "eolas-library"),
    "3" = {
      p <- readline("Enter path: ")
      if (nzchar(p)) path.expand(p) else NULL
    },
    "4" = path.expand("~/.cache/eolas"),
    NULL   # 0 = user cancelled (Esc / Ctrl-C) -> fall through
  )

  if (!is.null(resolved) && nzchar(resolved)) {
    suppressMessages(eolas_library_set(resolved))
    return(resolved)
  }
  NULL
}


# ---------------------------------------------------------------------------
# Public: resolve the library directory
# ---------------------------------------------------------------------------

#' @keywords internal
eolas_resolve_library_dir <- function() {
  # Step 2: EOLAS_LIBRARY env var
  env <- Sys.getenv("EOLAS_LIBRARY", unset = "")
  if (nzchar(env)) {
    return(normalizePath(path.expand(env), mustWork = FALSE))
  }

  # Step 3: config file
  cfg <- .eolas_read_library_dir_from_config()
  if (nzchar(cfg)) {
    return(normalizePath(path.expand(cfg), mustWork = FALSE))
  }

  # Step 4: interactive prompt (once per session, skipped in batch/CI/Rmd/Shiny).
  # Uses R's stdlib interactive() — the standard cross-platform TTY gate,
  # equivalent to Python's sys.stdin.isatty().  Called via .eolas_is_interactive()
  # so tests can mock it with local_mocked_bindings(.package = "eolas").
  if (.eolas_is_interactive() && !isTRUE(.eolas_lib_runtime$prompt_fired)) {
    prompted <- .eolas_prompt_library_dir()
    if (!is.null(prompted)) {
      return(normalizePath(path.expand(prompted), mustWork = FALSE))
    }
  }

  # Step 5: fallback (~/.cache/eolas) — non-interactive OR user cancelled.
  .eolas_emit_headless_info_once()
  normalizePath(path.expand("~/.cache/eolas"), mustWork = FALSE)
}


# ---------------------------------------------------------------------------
# Public helpers: eolas_library_set / eolas_library_status / eolas_library_clear
# ---------------------------------------------------------------------------

#' Set the eolas library directory
#'
#' Writes the chosen path to `~/.eolas/config.json` as `library_dir`.
#' Future calls to [eolas_get_local()] will use this directory when no
#' explicit `cache_dir` argument is passed.
#'
#' The config file is shared with the Python `eolas-data` client, so a path
#' set from R is immediately honoured in Python and vice versa.
#'
#' @param path Character string — the directory path to use as the library.
#'   Supports `~`-prefixed paths.
#' @return The resolved (absolute) path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' eolas_library_set("~/eolas-library")
#' eolas_library_set("/data/eolas")
#' }
eolas_library_set <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty string.", call. = FALSE)
  }
  resolved <- normalizePath(path.expand(path), mustWork = FALSE)
  .eolas_write_config_key("library_dir", resolved)
  cfg <- .eolas_config_file()
  cli::cli_alert_success(c(
    "library_dir set to {.path {resolved}}",
    "i" = "config file: {.path {cfg}}"
  ))
  invisible(resolved)
}


#' Show the resolved eolas library directory
#'
#' Checks all sources in precedence order and reports which one supplies the
#' library directory:
#'
#' 1. `EOLAS_LIBRARY` environment variable
#' 2. `library_dir` in `~/.eolas/config.json`
#' 3. `~/.cache/eolas/` (transient fallback)
#'
#' @return A named list with elements `source`, `path`, `env_var`,
#'   `config_file`, and `config_value`, invisibly.  Called primarily for
#'   its printed output.
#' @export
#' @examples
#' \dontrun{
#' eolas_library_status()
#' }
eolas_library_status <- function() {
  env <- Sys.getenv("EOLAS_LIBRARY", unset = "")
  cfg <- .eolas_read_library_dir_from_config()

  if (nzchar(env)) {
    source_label <- "env EOLAS_LIBRARY"
    resolved <- normalizePath(path.expand(env), mustWork = FALSE)
    source_key <- "env"
  } else if (nzchar(cfg)) {
    source_label <- .eolas_config_file()
    resolved <- normalizePath(path.expand(cfg), mustWork = FALSE)
    source_key <- "config"
  } else {
    source_label <- "fallback (transient — configure a library for reproducibility)"
    resolved <- normalizePath(path.expand("~/.cache/eolas"), mustWork = FALSE)
    source_key <- "fallback"
  }

  cli::cli_inform(c(
    "library: {.path {resolved}}",
    "source:  {source_label}"
  ))

  if (source_key == "fallback") {
    cli::cli_inform(c(
      "",
      "To set a persistent library:",
      "*" = "{.run [eolas_library_set(\"~/eolas-library\")](eolas::eolas_library_set())}",
      "*" = "{.code Sys.setenv(EOLAS_LIBRARY = \"/path/to/lib\")}"
    ))
  }

  invisible(list(
    source       = source_key,
    path         = resolved,
    env_var      = env,
    config_file  = .eolas_config_file(),
    config_value = cfg
  ))
}


#' Remove the library directory from the eolas config file
#'
#' Removes `library_dir` from `~/.eolas/config.json`.  After clearing,
#' [eolas_get_local()] falls back to `~/.cache/eolas/` (or the
#' `EOLAS_LIBRARY` env var if set).
#'
#' @return Invisibly `NULL`.
#' @export
#' @examples
#' \dontrun{
#' eolas_library_clear()
#' }
eolas_library_clear <- function() {
  .eolas_remove_config_key("library_dir")
  cfg <- .eolas_config_file()
  cli::cli_alert_success("library_dir removed from {.path {cfg}}")
  invisible(NULL)
}
