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


# -------------------------------------------------------------------------
# eolas_get_local — notebook-friendly whole-dataset convenience
# -------------------------------------------------------------------------

#' Download (or serve from cache) a whole dataset as a local data frame
#'
#' This is the recommended path for large or geospatial datasets in an
#' interactive R session or R Markdown notebook.  On the first call it fetches
#' the bulk file from CDN (milliseconds for monthly snapshots) and writes it to
#' `~/.cache/eolas/`.  On subsequent calls a lightweight HEAD request checks
#' whether the local file is still current; if so the cached copy is read
#' directly — zero network I/O on the data payload.
#'
#' If you have been calling `eolas_get("nz_parcels")` on a 3-million-row
#' geospatial dataset and it takes 15+ minutes, use `eolas_get_local()`
#' instead — it serves a pre-materialised GeoParquet from CDN, not a live
#' Iceberg scan through the row-oriented data endpoint.
#'
#' @section Format auto-detection:
#' When `format = NULL` (the default), `eolas_get_local()` calls
#' `eolas_info(name)` and checks the metadata for a `geometry_type` field.
#' Geo datasets use `"geoparquet"`; everything else uses `"parquet"`.
#'
#' @section GeoParquet and sf:
#' When `format = "geoparquet"` and the `sf` package is installed, the
#' returned object is an `sf` data frame with the CRS read from the GeoParquet
#' metadata (typically OGC:CRS84 / WGS84).  If `sf` is not installed, or
#' `as_sf = FALSE`, a plain data frame is returned with the WKT geometry
#' preserved as a character column (extracted from the WKB binary by the
#' `sfarrow` package if available, else left as raw).  Install `sf` with
#' `install.packages("sf")`.
#'
#' @param name Dataset identifier, e.g. `"nz_parcels"`.
#' @param cache_dir Local directory for cached files.  Accepts `~`-prefixed
#'   paths.  Created if it does not exist.  `NULL` (default) resolves via the
#'   library precedence chain: `EOLAS_LIBRARY` env var,
#'   then `library_dir` in `~/.eolas/config.json`,
#'   then `~/.cache/eolas/` fallback.  An explicit value here always wins
#'   (highest priority). Use `eolas_library_set()` to configure a persistent
#'   location.
#' @param format `"parquet"`, `"csv_gz"`, or `"geoparquet"`.  `NULL` (default)
#'   auto-detects from dataset metadata.
#' @param freshness `"auto"` (default), `"monthly"`, or `"current"`.  Passed
#'   verbatim to [eolas_sync_bulk()].
#' @param as_sf When `TRUE` (default) and the file is GeoParquet, the function
#'   attempts to return an `sf` object via `sf::st_read()` or
#'   `sfarrow::st_read_parquet()`.  When `FALSE`, a plain data frame is
#'   returned regardless of geometry.
#' @param base_url Override the API base URL (useful for testing).
#' @param ... Reserved for future arguments; currently ignored.
#' @return A `data.frame` or `sf` object, depending on the dataset and the
#'   `as_sf` argument.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # 3-million-row geospatial dataset — first call downloads GeoParquet from CDN;
#' # subsequent calls return in <1 s via sidecar check.
#' gdf <- eolas_get_local("nz_parcels")
#'
#' # Non-geo tabular dataset
#' df <- eolas_get_local("nz_cpi")
#'
#' # Explicit cache directory (overrides library config — highest priority)
#' df <- eolas_get_local("nz_cpi", cache_dir = "/data/eolas-cache")
#'
#' # Force CSV format
#' df <- eolas_get_local("nz_cpi", format = "csv_gz")
#'
#' # Keep plain data.frame even for geo datasets
#' df <- eolas_get_local("nz_parcels", as_sf = FALSE)
#' }
#' @seealso [eolas_sync_bulk()], `eolas_library_set()`, <https://docs.eolas.fyi/bulk-downloads/>
eolas_get_local <- function(name,
                             cache_dir = NULL,
                             format    = NULL,
                             freshness = "auto",
                             as_sf     = TRUE,
                             base_url  = EOLAS_BASE_URL,
                             ...) {

  # ---- argument validation --------------------------------------------------
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  freshness <- match.arg(freshness, .BULK_VALID_FRESHNESS)

  # ---- resolve cache_dir ---------------------------------------------------
  # Explicit cache_dir= wins (Step 1 of the precedence chain).
  # NULL triggers the library resolver (Steps 2-5).
  if (is.null(cache_dir)) {
    cache_dir_expanded <- eolas_resolve_library_dir()
  } else {
    cache_dir_expanded <- path.expand(as.character(cache_dir))
  }
  dir.create(cache_dir_expanded, recursive = TRUE, showWarnings = FALSE)
  cache_dir_abs <- tools::file_path_as_absolute(cache_dir_expanded)

  # ---- auto-detect format if not specified ----------------------------------
  if (is.null(format)) {
    meta   <- eolas_info(name, base_url = base_url)
    is_geo <- !is.null(meta$geometry_type) || !is.null(meta$geometry_wkt) ||
              isTRUE(meta$has_geometry)
    fmt    <- if (is_geo) "geoparquet" else "parquet"
  } else {
    fmt <- match.arg(format, .BULK_VALID_FORMATS)
  }

  # ---- compute local file path ----------------------------------------------
  ext       <- .BULK_EXTENSIONS[[fmt]]            # e.g. ".parquet", ".csv.gz", ".geo.parquet"
  file_path <- file.path(cache_dir_abs, paste0(name, ext))

  # ---- sync (download if needed, HEAD check if cached) --------------------
  # Bulk-specific stop() errors (Bulk upgrade required / Bulk licence
  # restricted / Bulk not yet available) propagate unchanged — their messages
  # already tell the user what to do.
  eolas_sync_bulk(name, path = file_path, format = fmt,
                  freshness = freshness, base_url = base_url)

  # ---- read the local file into a data frame --------------------------------
  if (fmt == "geoparquet" && isTRUE(as_sf)) {
    # Try sfarrow first (reads GeoParquet natively), then sf via a temp copy.
    if (requireNamespace("sfarrow", quietly = TRUE)) {
      return(sfarrow::st_read_parquet(file_path))
    }
    if (requireNamespace("sf", quietly = TRUE)) {
      return(sf::st_read(file_path, quiet = TRUE))
    }
    # Neither sf nor sfarrow available — fall through to plain read below.
    message(
      "eolas: sf or sfarrow package needed to return a GeoParquet as an sf object. ",
      "Install with install.packages(\"sf\"). Returning plain data.frame."
    )
  }

  # Plain read paths.
  if (fmt == "csv_gz") {
    return(utils::read.csv(gzfile(file_path), stringsAsFactors = FALSE))
  }

  # parquet or geoparquet-without-sf: use arrow if available, else error.
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(as.data.frame(arrow::read_parquet(file_path)))
  }

  stop(
    "The `arrow` package is required to read Parquet files in R. ",
    "Install with: install.packages(\"arrow\")",
    call. = FALSE
  )
}
