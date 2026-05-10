#' Generate connector configs for third-party data-pipeline tools
#'
#' Calls the eolas Enterprise-only `/v1/integrations/<platform>` endpoint and
#' returns the generated config files. Optionally writes them to disk.
#'
#' Supported platforms:
#' \itemize{
#'   \item `"meltano"` — `meltano.yml` using `tap-rest-api-msdk`, plus README
#'         and `.env.example`. `meltano install && meltano run tap-eolas
#'         target-jsonl` and you're loading.
#'   \item `"fivetran"` — Connector Builder YAML for paste-into-dashboard import.
#'   \item `"azure-data-factory"` — linked-service + per-dataset REST datasets
#'         + copy pipeline JSON; usable via `az datafactory` CLI or ADF Studio.
#' }
#'
#' This is an Enterprise-plan feature. Non-Enterprise keys receive a 403
#' from the server; the upgrade pointer flows through verbatim as the error
#' message. See <https://eolas.fyi/#pricing>.
#'
#' @param platform One of `"meltano"`, `"fivetran"`, `"azure-data-factory"`.
#' @param datasets Character vector of dataset names to include in the config.
#' @param output_dir Optional directory path. When supplied, the generated
#'   files are written there (creating the directory if needed) and the path
#'   to each written file is included in the returned list. When `NULL` (the
#'   default), files are returned in-memory only.
#' @param force When `output_dir` is set: overwrite existing files. Default
#'   `FALSE` skips files that already exist on disk.
#' @param base_url Override the API base URL (useful for testing).
#' @return A list with elements:
#'   \itemize{
#'     \item `platform` — the platform name as echoed by the server.
#'     \item `files` — a named list of `filename = content` (always populated).
#'     \item `written` — character vector of paths actually written
#'           (only present when `output_dir` is set).
#'     \item `skipped` — character vector of paths skipped because they
#'           already existed and `force = FALSE`.
#'   }
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_enterprise_key")
#'
#' # In-memory: inspect what the server would generate
#' result <- eolas_integration("meltano", c("nz_cpi", "nz_gdp"))
#' names(result$files)
#' cat(result$files$meltano.yml)
#'
#' # Write straight to a directory ready for `meltano install`
#' eolas_integration(
#'   "meltano",
#'   c("nz_cpi", "nz_gdp"),
#'   output_dir = "./my-pipeline"
#' )
#' }
eolas_integration <- function(platform,
                              datasets,
                              output_dir = NULL,
                              force = FALSE,
                              base_url = EOLAS_BASE_URL) {

  if (!is.character(platform) || length(platform) != 1L || !nzchar(platform)) {
    stop("`platform` must be a non-empty string.", call. = FALSE)
  }
  if (!is.character(datasets) || length(datasets) == 0L) {
    stop("`datasets` must be a non-empty character vector.", call. = FALSE)
  }

  resp <- eolas_http_get(
    paste0("/v1/integrations/", platform),
    datasets = paste(datasets, collapse = ","),
    base_url = base_url
  )
  body <- httr2::resp_body_json(resp)
  files <- body$files %||% list()

  result <- list(
    platform = body$platform %||% platform,
    files    = files
  )

  if (!is.null(output_dir)) {
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    written <- character(0)
    skipped <- character(0)
    for (filename in names(files)) {
      target <- file.path(output_dir, filename)
      if (file.exists(target) && !force) {
        skipped <- c(skipped, target)
        next
      }
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      writeLines(files[[filename]], target, sep = "")
      written <- c(written, target)
    }
    result$written <- written
    result$skipped <- skipped
  }

  invisible(result)
}
