test_that("resolve_library_dir: EOLAS_LIBRARY env var takes precedence over config", {
  withr::with_envvar(list(EOLAS_LIBRARY = tempdir()), {
    result <- eolas_resolve_library_dir()
    expect_equal(result, normalizePath(tempdir(), mustWork = FALSE))
  })
})

test_that("resolve_library_dir: config file library_dir used when env var absent", {
  tmp <- withr::local_tempdir()
  cfg_path <- file.path(tmp, "config.json")
  lib_dir  <- file.path(tmp, "my-lib")

  writeLines(
    jsonlite::toJSON(list(library_dir = lib_dir), auto_unbox = TRUE),
    cfg_path
  )

  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file = function() cfg_path,
      .package = "eolas"
    )
    result <- eolas_resolve_library_dir()
    expect_equal(result, normalizePath(lib_dir, mustWork = FALSE))
  })
})

test_that("resolve_library_dir: falls back to ~/.cache/eolas when nothing configured", {
  tmp <- withr::local_tempdir()
  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file = function() file.path(tmp, "no_config.json"),
      .package = "eolas"
    )
    result <- eolas_resolve_library_dir()
    expected <- normalizePath(path.expand("~/.cache/eolas"), mustWork = FALSE)
    expect_equal(result, expected)
  })
})

test_that("eolas_library_set writes library_dir to config and returns resolved path", {
  tmp <- withr::local_tempdir()
  lib_path <- file.path(tmp, "eolas-lib")
  cfg_path <- file.path(tmp, "config.json")

  local_mocked_bindings(
    .eolas_config_file = function() cfg_path,
    .package = "eolas"
  )

  result <- suppressMessages(eolas_library_set(lib_path))

  expect_equal(result, normalizePath(lib_path, mustWork = FALSE))
  cfg <- jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = TRUE)
  expect_equal(cfg[["library_dir"]], normalizePath(lib_path, mustWork = FALSE))
})

test_that("eolas_library_clear removes library_dir from config", {
  tmp <- withr::local_tempdir()
  cfg_path <- file.path(tmp, "config.json")
  writeLines(
    jsonlite::toJSON(list(api_key = "vs_test", library_dir = "/some/path"), auto_unbox = TRUE),
    cfg_path
  )

  local_mocked_bindings(
    .eolas_config_file = function() cfg_path,
    .package = "eolas"
  )

  suppressMessages(eolas_library_clear())

  cfg <- jsonlite::fromJSON(readLines(cfg_path, warn = FALSE), simplifyVector = TRUE)
  expect_null(cfg[["library_dir"]])
  # api_key should be preserved
  expect_equal(cfg[["api_key"]], "vs_test")
})

test_that("eolas_library_status reports correct source when config supplies the dir", {
  tmp <- withr::local_tempdir()
  lib_path <- file.path(tmp, "lib-from-config")
  cfg_path <- file.path(tmp, "config.json")
  writeLines(
    jsonlite::toJSON(list(library_dir = lib_path), auto_unbox = TRUE),
    cfg_path
  )

  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file = function() cfg_path,
      .package = "eolas"
    )
    info <- suppressMessages(eolas_library_status())
    expect_equal(info$source, "config")
    expect_equal(info$path, normalizePath(lib_path, mustWork = FALSE))
  })
})

test_that("eolas_library_status reports 'env' when EOLAS_LIBRARY is set", {
  env_path <- normalizePath(tempdir(), mustWork = FALSE)
  withr::with_envvar(list(EOLAS_LIBRARY = env_path), {
    info <- suppressMessages(eolas_library_status())
    expect_equal(info$source, "env")
    expect_equal(info$path, env_path)
  })
})

test_that("eolas_library_status reports 'fallback' when nothing configured", {
  tmp <- withr::local_tempdir()
  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file = function() file.path(tmp, "no_config.json"),
      .package = "eolas"
    )
    info <- suppressMessages(eolas_library_status())
    expect_equal(info$source, "fallback")
    expect_match(info$path, ".cache", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# Interactive prompt tests
# ---------------------------------------------------------------------------

test_that("interactive prompt skipped in non-interactive sessions", {
  tmp <- withr::local_tempdir()

  # Reset the session-once gate so we start clean for this test.
  # Use assign() into the env object — the namespace binding itself is locked
  # (sealed namespace) but the env's fields are mutable.
  ns <- getNamespace("eolas")
  rt <- ns$.eolas_lib_runtime
  old_fired <- rt$prompt_fired
  assign("prompt_fired", FALSE, envir = rt)
  on.exit(assign("prompt_fired", old_fired, envir = rt), add = TRUE)

  menu_called <- FALSE

  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file    = function() file.path(tmp, "no_config.json"),
      .eolas_is_interactive = function() FALSE,
      .package = "eolas"
    )
    # .eolas_cli_select should never be reached when .eolas_is_interactive() is FALSE
    local_mocked_bindings(
      .eolas_cli_select = function(...) { menu_called <<- TRUE; 0L },
      .package = "eolas"
    )
    result <- suppressMessages(eolas_resolve_library_dir())
    expected <- normalizePath(path.expand("~/.cache/eolas"), mustWork = FALSE)
    expect_equal(result, expected)
    expect_false(menu_called, info = ".eolas_cli_select must not be called in non-interactive mode")
  })
})

test_that("interactive prompt fires when no config and session is interactive", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  rt <- ns$.eolas_lib_runtime
  old_fired <- rt$prompt_fired
  assign("prompt_fired", FALSE, envir = rt)
  on.exit(assign("prompt_fired", old_fired, envir = rt), add = TRUE)

  # Capture what eolas_library_set was called with
  set_called_with <- NULL

  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file    = function() file.path(tmp, "config.json"),
      .eolas_is_interactive = function() TRUE,
      eolas_library_set     = function(path) {
        set_called_with <<- path
        normalizePath(path.expand(path), mustWork = FALSE)
      },
      .package = "eolas"
    )
    local_mocked_bindings(
      .eolas_cli_select = function(...) 1L,   # choice 1 = ~/eolas-library
      .package = "eolas"
    )
    result <- suppressMessages(eolas_resolve_library_dir())

    expected_path <- normalizePath(path.expand("~/eolas-library"), mustWork = FALSE)
    expect_equal(result, expected_path)
    expect_equal(
      normalizePath(path.expand(set_called_with), mustWork = FALSE),
      expected_path,
      info = "eolas_library_set should have been called with ~/eolas-library"
    )
  })
})

test_that("interactive prompt only fires once per session", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  rt <- ns$.eolas_lib_runtime
  old_fired <- rt$prompt_fired
  assign("prompt_fired", FALSE, envir = rt)
  on.exit(assign("prompt_fired", old_fired, envir = rt), add = TRUE)

  menu_call_count <- 0L

  withr::with_envvar(list(EOLAS_LIBRARY = ""), {
    local_mocked_bindings(
      .eolas_config_file    = function() file.path(tmp, "no_config.json"),
      .eolas_is_interactive = function() TRUE,
      eolas_library_set     = function(path) normalizePath(path.expand(path), mustWork = FALSE),
      .package = "eolas"
    )
    local_mocked_bindings(
      .eolas_cli_select = function(...) { menu_call_count <<- menu_call_count + 1L; 1L },
      .package = "eolas"
    )

    # First call: prompt should fire
    suppressMessages(eolas_resolve_library_dir())
    expect_equal(menu_call_count, 1L, info = "menu should be called on first resolve")

    # Second call: prompt_fired is now TRUE; menu must NOT be called again.
    # .eolas_config_file and interactive() are still mocked from the outer scope.
    suppressMessages(eolas_resolve_library_dir())
    expect_equal(menu_call_count, 1L, info = "menu must not be called a second time in the same session")
  })
})

# ---------------------------------------------------------------------------

test_that("eolas_get_local: explicit cache_dir overrides library resolution", {
  explicit_dir <- withr::local_tempdir()
  written_dirs <- character(0)

  local_mocked_bindings(
    eolas_sync_bulk = function(name, path, format, freshness, progress = NULL, base_url, ...) {
      written_dirs <<- c(written_dirs, dirname(path))
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      writeBin(raw(0), path)
      list(
        status               = "downloaded",
        previous_snapshot_id = NA,
        current_snapshot_id  = "snap1",
        path                 = path,
        bytes_downloaded     = 0L
      )
    },
    eolas_info = function(name, base_url) list(name = name),
    .package = "eolas"
  )

  # Suppress stop from missing arrow; we only care about the path, not the read
  tryCatch(
    eolas_get_local("nz_cpi", cache_dir = explicit_dir, as_sf = FALSE),
    error = function(e) invisible(NULL)
  )

  expect_true(length(written_dirs) > 0L)
  expect_equal(
    normalizePath(written_dirs[[1L]], mustWork = FALSE),
    normalizePath(explicit_dir, mustWork = FALSE)
  )
})
