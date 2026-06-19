#!/usr/bin/env Rscript
# Verify parity scenario tests from eolas/docs/client-contract.md exist locally.

patterns <- list(
  "test-progress.R" = c(
    "eolas_resp_content_length",
    "eolas_download_progress_format"
  ),
  "test-bulk.R" = c(
    "eolas_sync_bulk force=TRUE",
    "eolas_cache_clear removes cached bulk files"
  ),
  "test-api.R" = c(
    "eolas_get forwards force to auto-routed",
    "eolas_get_linz forwards progress"
  ),
  "smoke-live.R" = c(
    "eolas_get_linz(\"nz_addresses\"",
    "eolas_cache_clear"
  )
)

errors <- character(0)
test_dir <- file.path("tests", "testthat")
for (fname in names(patterns)) {
  path <- if (fname == "smoke-live.R") {
    file.path("tests", fname)
  } else {
    file.path(test_dir, fname)
  }
  if (!file.exists(path)) {
    errors <- c(errors, sprintf("missing test file: %s", path))
    next
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  for (pat in patterns[[fname]]) {
    if (!grepl(pat, text, fixed = TRUE)) {
      errors <- c(errors, sprintf("%s: missing pattern %s", fname, shQuote(pat)))
    }
  }
}

if (length(errors)) {
  cat("Client contract check FAILED (see eolas/docs/client-contract.md):\n")
  for (e in errors) cat("  -", e, "\n")
  quit(status = 1)
}

cat("Client contract check OK (R)\n")