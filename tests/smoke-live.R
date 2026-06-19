#!/usr/bin/env Rscript
# Live API smoke — mirrors tests/test_smoke_live.py and eolas/docs/client-contract.md
#
# Run locally:
#   EOLAS_API_KEY=vs_... Rscript tests/smoke-live.R
#
# CI: .github/workflows/smoke.yml installs the package with R CMD INSTALL first.

key <- Sys.getenv("EOLAS_API_KEY", unset = "")
if (!nzchar(key)) {
  message("EOLAS_API_KEY not set — live smoke skipped.")
  quit(status = 0)
}

lib <- tempfile("eolas-smoke-")
dir.create(lib, recursive = TRUE)
Sys.setenv(EOLAS_LIBRARY = lib)

suppressPackageStartupMessages(library(eolas))

message("R live smoke: list + nz_cpi slice ...")
stopifnot(nrow(eolas_list()) >= 100L)

df <- eolas_get("nz_cpi", limit = 5L)
stopifnot(nrow(df) >= 1L, "value" %in% names(df))

meta <- eolas_info("nz_cpi")
stopifnot(identical(meta$name[[1]], "nz_cpi"))

stopifnot("eolas_cache_clear" %in% getNamespaceExports("eolas"))

message("R live smoke: eolas_get_linz(nz_addresses) bulk route + head() ...")
gdf <- eolas_get_linz("nz_addresses", progress = FALSE)
stopifnot(nrow(gdf) > 100000L)

# Must not error — geo metadata + repr regressions
print(utils::head(gdf, 3))

message("R live smoke: OK")