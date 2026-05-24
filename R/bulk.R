# Bulk download — wraps GET /v1/bulk/{namespace}/{table}
#
# The endpoint requires both namespace and table, which the server knows but
# the user addresses only by name. We resolve name → namespace + table with a
# quick GET /v1/datasets/{name} call (already wrapped as eolas_info()), then
# fetch the binary file.

# Valid format strings accepted by the server bulk endpoint.
.BULK_VALID_FORMATS    <- c("parquet", "csv_gz", "geoparquet")
.BULK_VALID_FRESHNESS  <- c("auto", "monthly", "current")

# Default output-file extensions for each format.
.BULK_EXTENSIONS <- c(
  parquet    = ".parquet",
  csv_gz     = ".csv.gz",
  geoparquet = ".geo.parquet"
)


#' Download a complete dataset as a single file
#'
#' Wraps `GET /v1/bulk/{namespace}/{table}` to download a whole Iceberg table
#' as a Parquet, gzipped-CSV, or GeoParquet snapshot — no row caps, no
#' pagination.
#'
#' The endpoint requires both `namespace` and `table`. These are resolved
#' automatically by calling `GET /v1/datasets/{name}` first and reading the
#' metadata. The extra round-trip is negligible; monthly snapshots are served
#' from Cloudflare's edge cache in milliseconds.
#'
#' @section Freshness:
#' `freshness = "auto"` (the default) omits the query parameter so the server
#' redirects to the right level for your plan — Free accounts get the latest
#' monthly snapshot; Pro accounts get the current Iceberg snapshot. Pass
#' `"monthly"` or `"current"` to override explicitly.
#'
#' @section Formats:
#' \describe{
#'   \item{`"parquet"`}{Apache Parquet — best for R (via the `arrow` package),
#'     Polars, DuckDB, Spark.}
#'   \item{`"csv_gz"`}{Gzipped CSV — readable by `read.csv()`,
#'     `readr::read_csv()`, Excel.}
#'   \item{`"geoparquet"`}{GeoParquet 1.0 — only available on datasets with
#'     geometry; read with `sfarrow::st_read_parquet()` or `geopandas`.}
#' }
#'
#' @section Error conditions:
#' \describe{
#'   \item{HTTP 402}{Stops with `"Bulk upgrade required:"` — `freshness = "current"`
#'     requires a Pro plan.}
#'   \item{HTTP 403 (licence)}{Stops with `"Bulk licence restricted:"` — dataset is
#'     excluded from bulk (e.g. OECD). Use `eolas_get()` instead.}
#'   \item{HTTP 503}{Stops with `"Bulk not yet available:"` — monthly snapshot
#'     not yet generated.}
#' }
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param freshness `"auto"` (default), `"monthly"`, or `"current"`.
#'   `"auto"` lets the server choose based on your plan.
#' @param format `"parquet"` (default), `"csv_gz"`, or `"geoparquet"`.
#' @param path Where to write the file. `NULL` (default) returns the raw
#'   bytes as a raw vector. A file path writes the file and returns its
#'   normalised path invisibly.
#' @param base_url Override the API base URL (useful for testing).
#' @param ... Reserved for future arguments; currently ignored.
#' @return Invisibly the normalised path when `path` is set;
#'   a raw vector when `path = NULL`.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # Return raw bytes (e.g. hand to arrow::read_parquet)
#' raw_bytes <- eolas_download_bulk("nz_cpi")
#' df <- arrow::read_parquet(raw_bytes)
#'
#' # Write to a file, get the path back
#' path <- eolas_download_bulk("nz_cpi", path = "nz_cpi.parquet")
#' df <- arrow::read_parquet(path)
#'
#' # Gzipped CSV (readable by read.csv)
#' eolas_download_bulk("nz_cpi", format = "csv_gz", path = "nz_cpi.csv.gz")
#' df <- read.csv(gzfile("nz_cpi.csv.gz"))
#'
#' # Force monthly freshness (reproducibility)
#' eolas_download_bulk("nz_cpi", freshness = "monthly", path = "nz_cpi.parquet")
#'
#' # GeoParquet for a geospatial dataset
#' eolas_download_bulk("territorial_authority_2023",
#'                     format = "geoparquet",
#'                     path   = "ta2023.geo.parquet")
#' }
#'
#' @seealso
#' <https://docs.eolas.fyi/bulk-downloads/>
eolas_download_bulk <- function(name,
                                freshness = "auto",
                                format    = "parquet",
                                path      = NULL,
                                base_url  = EOLAS_BASE_URL,
                                ...) {

  # ---- argument validation --------------------------------------------------
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  format    <- match.arg(format,    .BULK_VALID_FORMATS)
  freshness <- match.arg(freshness, .BULK_VALID_FRESHNESS)

  # ---- resolve name → namespace + table ------------------------------------
  meta      <- eolas_info(name, base_url = base_url)
  namespace <- meta$namespace %||% ""
  table     <- meta$table %||% meta$name %||% name

  if (!nzchar(namespace)) {
    stop(
      "Dataset ", sQuote(name), " metadata did not include a namespace field. ",
      "Cannot construct bulk URL.",
      call. = FALSE
    )
  }

  # ---- build query params ---------------------------------------------------
  query <- list(format = format)
  if (freshness != "auto") query$freshness <- freshness

  # ---- perform the request --------------------------------------------------
  key <- eolas_get_key_internal()
  url <- paste0(base_url, "/v1/bulk/", namespace, "/", table)
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_error(is_error = \(r) FALSE)

  resp <- eolas_http_perform(req)

  # ---- bulk-specific status handling ----------------------------------------
  status <- httr2::resp_status(resp)

  if (status == 402L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% paste0(
      "Fresh bulk downloads are a Pro feature. Free accounts get the latest ",
      "monthly snapshot — see https://eolas.fyi/pricing."
    )
    stop("Bulk upgrade required: ", detail, call. = FALSE)
  }

  if (status == 403L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      stop("Bulk licence restricted: ", detail, call. = FALSE)
    }
    # Key-auth 403 — delegate to the standard status handler.
    eolas_check_status(resp)
  }

  if (status == 503L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% paste0(
      "Monthly bulk snapshots are still rolling out for this dataset. ",
      "Try again after the 1st of next month, or upgrade to Pro for ",
      "on-demand current snapshots — see https://eolas.fyi/pricing."
    )
    stop("Bulk not yet available: ", detail, call. = FALSE)
  }

  # All other non-200 codes (401, 404, 429, 5xx) go through the standard handler.
  if (status != 200L) {
    eolas_check_status(resp)
  }

  # ---- decode body ----------------------------------------------------------
  raw_bytes <- httr2::resp_body_raw(resp)

  # ---- write or return ------------------------------------------------------
  if (is.null(path)) {
    return(raw_bytes)
  }

  out_path <- normalizePath(path, mustWork = FALSE)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeBin(raw_bytes, out_path)
  invisible(out_path)
}
