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

# Sidecar schema version — bump if the JSON structure changes incompatibly.
.SIDECAR_SCHEMA_VERSION <- 1L


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


# -------------------------------------------------------------------------
# Internal helpers for eolas_sync_bulk
# -------------------------------------------------------------------------

.read_sidecar <- function(sidecar_path) {
  tryCatch(
    jsonlite::fromJSON(readLines(sidecar_path, warn = FALSE), simplifyVector = TRUE),
    error = function(e) NULL
  )
}

.write_sidecar <- function(sidecar_path, name, snapshot_id, fmt, freshness, source_url) {
  data <- list(
    schema_version = .SIDECAR_SCHEMA_VERSION,
    name           = name,
    snapshot_id    = snapshot_id,
    format         = fmt,
    freshness      = freshness,
    downloaded_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_url     = source_url
  )
  writeLines(jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE), sidecar_path)
}

# HEAD the bulk endpoint to read X-Snapshot-Version without downloading data.
.head_snapshot_version <- function(url, query, key) {
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- eolas_http_perform(req)
  # Bulk refusal codes on HEAD must still propagate.
  status <- httr2::resp_status(resp)
  if (status == 402L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% "Fresh bulk downloads are a Pro feature."
    stop("Bulk upgrade required: ", detail, call. = FALSE)
  }
  if (status == 403L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      stop("Bulk licence restricted: ", detail, call. = FALSE)
    }
    eolas_check_status(resp)
  }
  if (status == 503L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% "Monthly bulk snapshots are still rolling out."
    stop("Bulk not yet available: ", detail, call. = FALSE)
  }
  if (status != 200L) {
    eolas_check_status(resp)
  }
  # Return the X-Snapshot-Version header value (empty string if absent).
  headers <- httr2::resp_headers(resp)
  headers[["X-Snapshot-Version"]] %||% ""
}


#' Incrementally sync a bulk dataset file
#'
#' Checks whether the locally-cached file is still current by issuing a
#' lightweight HEAD request and reading the `X-Snapshot-Version` response
#' header.  If the snapshot id matches the sidecar, the function returns
#' immediately with `status = "unchanged"` and no data I/O.  Otherwise it
#' downloads the new snapshot, replaces the local file **atomically** (via a
#' temp file + `file.rename()`), and updates the sidecar.
#'
#' @section Sidecar:
#' A JSON file `<path>.eolas-meta.json` is written next to the data file.
#' It stores the snapshot id, download timestamp, format, and source URL
#' and is read on the next call to perform the no-op check cheaply.
#'
#' @section Atomic replacement:
#' The new file is downloaded to `<path>.eolas-tmp-<rand>` and then renamed
#' over the original with `file.rename()`.  On most POSIX systems this is an
#' atomic inode swap; on Windows it uses `MoveFileExW` with
#' `MOVEFILE_REPLACE_EXISTING`.  Readers with the file open will see either
#' the old or the new content, never a partial write.
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param path **Required.** File path where the data should live.  The
#'   sidecar is written at `paste0(path, ".eolas-meta.json")`.
#'   Parent directories are created automatically.
#' @param format `"parquet"` (default), `"csv_gz"`, or `"geoparquet"`.
#' @param freshness `"auto"` (default), `"monthly"`, or `"current"`.
#' @param base_url Override the API base URL (useful for testing).
#' @param ... Reserved; currently ignored.
#' @return A named list with the same fields as Python's `SyncResult`:
#' \describe{
#'   \item{`status`}{`"downloaded"`, `"updated"`, or `"unchanged"`}
#'   \item{`previous_snapshot_id`}{Snapshot id from the sidecar, or `NA` if none}
#'   \item{`current_snapshot_id`}{Snapshot id from the server}
#'   \item{`path`}{Normalised path to the data file}
#'   \item{`bytes_downloaded`}{Bytes written (0 when unchanged)}
#' }
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # First call: full download
#' r <- eolas_sync_bulk("nz_cpi", path = "nz_cpi.parquet")
#' r$status           # "downloaded"
#' r$bytes_downloaded # e.g. 2100000
#'
#' # Second call (same snapshot): no network I/O on the data file
#' r <- eolas_sync_bulk("nz_cpi", path = "nz_cpi.parquet")
#' r$status           # "unchanged"
#' r$bytes_downloaded # 0
#'
#' # Poll for updates in a long-running script
#' repeat {
#'   r <- eolas_sync_bulk("nz_cpi", path = "nz_cpi.parquet")
#'   if (r$status != "unchanged") message("Updated to snapshot ", r$current_snapshot_id)
#'   Sys.sleep(3600)
#' }
#' }
#' @seealso \code{\link{eolas_download_bulk}}, <https://docs.eolas.fyi/bulk-downloads/>
eolas_sync_bulk <- function(name,
                            path,
                            format    = "parquet",
                            freshness = "auto",
                            base_url  = EOLAS_BASE_URL,
                            ...) {

  # ---- argument validation --------------------------------------------------
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  if (missing(path) || is.null(path)) {
    stop("`path` is required for eolas_sync_bulk().", call. = FALSE)
  }
  format    <- match.arg(format,    .BULK_VALID_FORMATS)
  freshness <- match.arg(freshness, .BULK_VALID_FRESHNESS)

  out_path    <- normalizePath(path, mustWork = FALSE)
  sidecar_path <- paste0(out_path, ".eolas-meta.json")

  # ---- read local sidecar ---------------------------------------------------
  prev <- if (file.exists(sidecar_path)) .read_sidecar(sidecar_path) else NULL

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

  # ---- HEAD to get X-Snapshot-Version cheaply -------------------------------
  key <- eolas_get_key_internal()
  bulk_url <- paste0(base_url, "/v1/bulk/", namespace, "/", table)
  current_sid <- .head_snapshot_version(bulk_url, query, key)

  # ---- no-op fast path ------------------------------------------------------
  prev_sid <- if (!is.null(prev)) prev$snapshot_id %||% NA_character_ else NA_character_
  if (!is.na(prev_sid) &&
      identical(prev_sid, current_sid) &&
      file.exists(out_path)) {
    return(list(
      status               = "unchanged",
      previous_snapshot_id = prev_sid,
      current_snapshot_id  = current_sid,
      path                 = out_path,
      bytes_downloaded     = 0L
    ))
  }

  # ---- download (atomic replace) --------------------------------------------
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  rand_hex  <- paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")
  tmp_path  <- paste0(out_path, ".eolas-tmp-", rand_hex)

  req <- httr2::request(bulk_url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_error(is_error = \(r) FALSE)

  resp <- eolas_http_perform(req)

  # ---- bulk-specific status handling (mirrors eolas_download_bulk) ----------
  status <- httr2::resp_status(resp)

  if (status == 402L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% "Fresh bulk downloads are a Pro feature."
    stop("Bulk upgrade required: ", detail, call. = FALSE)
  }
  if (status == 403L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      stop("Bulk licence restricted: ", detail, call. = FALSE)
    }
    eolas_check_status(resp)
  }
  if (status == 503L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% "Monthly bulk snapshots are still rolling out."
    stop("Bulk not yet available: ", detail, call. = FALSE)
  }
  if (status != 200L) {
    eolas_check_status(resp)
  }

  raw_bytes   <- httr2::resp_body_raw(resp)
  bytes_dl    <- length(raw_bytes)

  # Write to tmp, then atomically rename onto the destination.
  writeBin(raw_bytes, tmp_path)
  ok <- file.rename(tmp_path, out_path)
  if (!ok) {
    # file.rename can fail across filesystems — fall back to copy + unlink.
    file.copy(tmp_path, out_path, overwrite = TRUE)
    unlink(tmp_path)
  }

  # ---- write sidecar --------------------------------------------------------
  source_url <- paste0(
    bulk_url, "?format=", format,
    if (freshness != "auto") paste0("&freshness=", freshness) else ""
  )
  .write_sidecar(sidecar_path, name, current_sid, format, freshness, source_url)

  list(
    status               = if (is.null(prev) || is.na(prev_sid)) "downloaded" else "updated",
    previous_snapshot_id = prev_sid,
    current_snapshot_id  = current_sid,
    path                 = out_path,
    bytes_downloaded     = bytes_dl
  )
}
