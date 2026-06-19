# Bulk download -- wraps GET /v1/bulk/{namespace}/{table}
#
# The endpoint requires both namespace and table, which the server knows but
# the user addresses only by name. We resolve name -> namespace + table with a
# quick GET /v1/datasets/{name} call (already wrapped as eolas_info()), then
# fetch the binary file.

# Valid format strings accepted by the server bulk endpoint.
.BULK_VALID_FORMATS    <- c("parquet", "csv_gz", "geoparquet")
.BULK_VALID_FRESHNESS  <- c("auto", "monthly", "current")

# ---------------------------------------------------------------------------
# Internal TTY gate -- thin wrapper so tests can mock it cleanly via
# with_mocked_bindings(.package = "eolas").
# ---------------------------------------------------------------------------
.eolas_is_interactive <- function() interactive()

# Internal streaming gate -- returns TRUE to use req_perform_connection()
# (streaming, chunk-by-chunk progress), FALSE to use eolas_http_perform()
# (buffered, for test mocks that return a pre-built response).
# Tests mock this to FALSE via with_mocked_bindings(.package = "eolas").
.eolas_use_streaming <- function() TRUE

# Resolve the tri-state `progress` argument to download/read logicals.
# `progress` may be:
#   NULL     -- auto (both phases follow interactive() / EOLAS_NO_PROGRESS)
#   TRUE     -- both phases on
#   FALSE    -- both phases off
#   "both"   -- both on (alias of TRUE)
#   "download" -- byte stream bar only (network)
#   "read"   -- disk-load spinner only (Parquet/sf materialisation)
#   "none"   -- both off (alias of FALSE)
.eolas_resolve_progress_phases <- function(progress) {
  if (identical(progress, FALSE)) {
    return(list(download = FALSE, read = FALSE))
  }
  if (identical(progress, TRUE)) {
    return(list(download = TRUE, read = TRUE))
  }
  if (is.character(progress) && length(progress) == 1L && nzchar(progress)) {
    p <- tolower(trimws(progress))
    return(switch(p,
      both     = list(download = TRUE,  read = TRUE),
      all      = list(download = TRUE,  read = TRUE),
      download = list(download = TRUE,  read = FALSE),
      read     = list(download = FALSE, read = TRUE),
      none     = list(download = FALSE, read = FALSE),
      stop(
        "`progress` must be TRUE, FALSE, NULL, or one of ",
        '"both", "download", "read", "none".',
        call. = FALSE
      )
    ))
  }
  auto <- {
    env_val <- trimws(Sys.getenv("EOLAS_NO_PROGRESS", unset = ""))
    if (env_val %in% c("1", "true", "yes")) FALSE else .eolas_is_interactive()
  }
  list(download = auto, read = auto)
}

# Resolve one phase. Used by download (streaming) and read (disk) paths.
.eolas_resolve_progress <- function(progress, phase = c("download", "read")) {
  phase <- match.arg(phase)
  .eolas_resolve_progress_phases(progress)[[phase]]
}

# Indeterminate cli spinner while materialising a cached bulk file.
.eolas_with_read_progress <- function(label, show, expr) {
  if (!isTRUE(show) || !requireNamespace("cli", quietly = TRUE)) {
    return(force(expr))
  }
  cli::cli_progress_bar(
    name   = label,
    total  = NA,
    format = "{cli::pb_spin} Loading {cli::pb_name} from disk\u2026",
    clear  = FALSE
  )
  on.exit(cli::cli_progress_done(), add = TRUE)
  force(expr)
}

# Stream the body of a performing httr2 connection-response to a file,
# optionally showing a cli progress bar.
#
# `resp`        -- connection response from req_perform_connection().
# `dest_path`   -- file path to write into (opened in binary mode).
# `total_bytes` -- expected size from Content-Length, or NA for unknown.
# `label`       -- short description shown in the bar (e.g. filename).
# `show_bar`    -- logical; TRUE -> show cli bar, FALSE -> silent.
#
# Returns the number of bytes written.
.eolas_resp_content_length <- function(resp) {
  val <- tryCatch(httr2::resp_header(resp, "Content-Length"), error = function(e) NULL)
  if (is.null(val) || length(val) == 0L || is.na(val[[1L]]) || !nzchar(val[[1L]])) {
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(val[[1L]]))
  if (length(out) != 1L || is.na(out)) NA_real_ else out
}

.eolas_download_progress_format <- function(total_bytes) {
  known_total <- length(total_bytes) == 1L && !is.na(total_bytes)
  if (known_total) {
    paste0(
      "{cli::pb_name} ",
      "{cli::pb_current_bytes}/{cli::pb_total_bytes} ",
      "{cli::pb_rate_bytes} ETA {cli::pb_eta}"
    )
  } else {
    # CDN often omits Content-Length -- never reference pb_total_bytes/pb_eta then.
    paste0(
      "{cli::pb_name} ",
      "{cli::pb_current_bytes} ",
      "{cli::pb_rate_bytes}"
    )
  }
}

.eolas_stream_to_file <- function(resp, dest_path, total_bytes, label, show_bar) {
  CHUNK <- 1048576L  # 1 MiB per chunk

  has_cli <- requireNamespace("cli", quietly = TRUE)
  if (length(total_bytes) != 1L) total_bytes <- NA_real_
  known_total <- length(total_bytes) == 1L && !is.na(total_bytes)

  if (show_bar && has_cli) {
    bar_id <- cli::cli_progress_bar(
      name   = label,
      total  = if (known_total) total_bytes else NA,
      format = .eolas_download_progress_format(total_bytes),
      clear  = FALSE
    )
    on.exit(cli::cli_progress_done(id = bar_id), add = TRUE)
  }

  bytes_written <- 0L
  fh <- file(dest_path, open = "wb")
  on.exit(close(fh), add = TRUE)

  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = CHUNK %/% 1024L)
    if (length(chunk) == 0L) break
    writeBin(chunk, fh)
    bytes_written <- bytes_written + length(chunk)
    if (show_bar && has_cli) {
      cli::cli_progress_update(inc = length(chunk), id = bar_id)
    }
  }

  bytes_written
}

# Default output-file extensions for each format.
.BULK_EXTENSIONS <- c(
  parquet    = ".parquet",
  csv_gz     = ".csv.gz",
  geoparquet = ".geo.parquet"
)

# Infer parquet vs geoparquet from dataset metadata (shared by get_local / cache_clear).
.eolas_detect_bulk_format <- function(meta, name, base_url) {
  if (is.null(meta)) meta <- eolas_info(name, base_url = base_url)
  gt        <- if ("geometry_type" %in% names(meta)) meta$geometry_type[[1]] else NULL
  wkt       <- if ("geometry_wkt" %in% names(meta)) meta$geometry_wkt[[1]] else NULL
  gt_truthy <- !is.null(gt)  && nzchar(gt)  && gt  != "none"
  wkt_truthy <- !is.null(wkt) && nzchar(wkt) && wkt != "none"
  has_geom  <- if ("has_geometry" %in% names(meta)) isTRUE(meta$has_geometry[[1]]) else FALSE
  if (gt_truthy || wkt_truthy || has_geom) "geoparquet" else "parquet"
}

# Local bulk-cache paths for one dataset under a library directory.
.eolas_bulk_cache_paths <- function(name, cache_dir, format = NULL,
                                    base_url = EOLAS_BASE_URL) {
  if (is.null(format)) {
    fmt <- .eolas_detect_bulk_format(NULL, name, base_url)
  } else {
    fmt <- match.arg(format, .BULK_VALID_FORMATS)
  }
  ext <- .BULK_EXTENSIONS[[fmt]]
  data_path <- file.path(cache_dir, paste0(name, ext))
  list(
    format       = fmt,
    data_path    = data_path,
    sidecar_path = paste0(data_path, ".eolas-meta.json")
  )
}

# Thin wrapper around sfarrow::st_read_parquet() -- exists solely so tests can
# mock it via local_mocked_bindings(.env = getNamespace("eolas")) without
# having to patch the sfarrow namespace directly.
.eolas_sfarrow_read_parquet <- function(file_path) {
  sfarrow::st_read_parquet(file_path)
}

# Read a `.geo.parquet` via `arrow::read_parquet()` and decode the binary WKB
# `geometry` column into an `sf` object -- handling zero-length WKB elements
# (empty/null source geometries -- e.g. 21% of LINZ nz_parcels) which
# `sf::st_as_sfc.WKB` would otherwise abort on with
# "cannot read WKB object from zero-length raw vector".
#
# Replaces sfarrow as the primary reader because sfarrow doesn't tolerate
# empty WKB rows and is also effectively unmaintained (no releases in ~3 yr).
# Same memory profile as sfarrow when sfarrow works; significantly faster
# than the WKT-string fallback path. See [[project_geoparquet_evolution]] memo.
.eolas_arrow_wkb_to_sf <- function(file_path) {
  tbl <- arrow::read_parquet(file_path)
  if (!"geometry" %in% names(tbl)) {
    stop(".eolas_arrow_wkb_to_sf: no 'geometry' column in ", file_path,
         call. = FALSE)
  }

  # arrow returns a list-vector of `arrow_binary` raw payloads -- convert to
  # a list of raw vectors (some will be length-0 for NULL source geometries).
  raw_list <- lapply(tbl$geometry, as.raw)
  non_empty <- vapply(raw_list, length, integer(1)) > 0L

  # Pre-allocate the sfc with universal-empty geometries, then slot decoded
  # WKB into the non-empty positions. GEOMETRYCOLLECTION EMPTY is sf's
  # canonical "valid but empty" sentinel and is type-agnostic.
  geom_list <- vector("list", length(raw_list))
  if (any(!non_empty)) {
    geom_list[!non_empty] <- list(sf::st_geometrycollection())
  }
  if (any(non_empty)) {
    decoded <- sf::st_as_sfc(
      structure(raw_list[non_empty], class = "WKB"),
      crs = 4326
    )
    geom_list[non_empty] <- decoded
  }
  sfc <- sf::st_sfc(geom_list, crs = 4326)

  # Drop the binary column from the attribute table, attach the decoded sfc
  attrs <- tibble::as_tibble(
    as.data.frame(tbl[, setdiff(names(tbl), "geometry"), drop = FALSE])
  )
  sf::st_sf(attrs, geometry = sfc)
}

# Sidecar schema version -- bump if the JSON structure changes incompatibly.
.SIDECAR_SCHEMA_VERSION <- 1L


#' Download a complete dataset as a single file
#'
#' Wraps `GET /v1/bulk/{namespace}/{table}` to download a whole Iceberg table
#' as a Parquet, gzipped-CSV, or GeoParquet snapshot -- no row caps, no
#' pagination.
#'
#' The endpoint requires both `namespace` and `table`. These are resolved
#' automatically by calling `GET /v1/datasets/{name}` first and reading the
#' metadata. The extra round-trip is negligible; monthly snapshots are served
#' from Cloudflare's edge cache in milliseconds.
#'
#' @section Freshness:
#' `freshness = "auto"` (the default) omits the query parameter so the server
#' redirects to the right level for your plan -- Free accounts get the latest
#' monthly snapshot; Pro accounts get the current Iceberg snapshot. Pass
#' `"monthly"` or `"current"` to override explicitly.
#'
#' @section Formats:
#' \describe{
#'   \item{`"parquet"`}{Apache Parquet -- best for R (via the `arrow` package),
#'     Polars, DuckDB, Spark.}
#'   \item{`"csv_gz"`}{Gzipped CSV -- readable by `read.csv()`,
#'     `readr::read_csv()`, Excel.}
#'   \item{`"geoparquet"`}{GeoParquet 1.0 -- only available on datasets with
#'     geometry; read with `sfarrow::st_read_parquet()` or `geopandas`.}
#' }
#'
#' @section Error conditions:
#' \describe{
#'   \item{HTTP 402}{Stops with `"Bulk upgrade required:"` -- `freshness = "current"`
#'     requires a Pro plan.}
#'   \item{HTTP 403 (licence)}{Stops with `"Bulk licence restricted:"` -- dataset is
#'     excluded from bulk (e.g. OECD). Use `eolas_get()` instead.}
#'   \item{HTTP 503}{Stops with `"Bulk not yet available:"` -- monthly snapshot
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
#' @param progress Control progress feedback. `NULL` (default) auto-detects both
#'   phases in interactive sessions. `TRUE`/`FALSE` force both on/off.
#'   Character selectors: `"download"` (network byte bar only),
#'   `"read"` (disk-load spinner only), `"both"`/`"none"`.
#'   Suppressed when `EOLAS_NO_PROGRESS=1`. Bytes mode (`path = NULL`) never
#'   shows a download bar.
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
#'
#' # Silence the bar in a script run interactively
#' eolas_download_bulk("nz_cpi", path = "nz_cpi.parquet", progress = FALSE)
#' }
#'
#' @seealso
#' <https://docs.eolas.fyi/bulk-downloads/>
eolas_download_bulk <- function(name,
                                freshness = "auto",
                                format    = "parquet",
                                path      = NULL,
                                progress  = NULL,
                                base_url  = EOLAS_BASE_URL,
                                ...) {

  # ---- argument validation --------------------------------------------------
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  format    <- match.arg(format,    .BULK_VALID_FORMATS)
  freshness <- match.arg(freshness, .BULK_VALID_FRESHNESS)

  # ---- resolve name -> namespace + table ------------------------------------
  meta      <- eolas_info(name, base_url = base_url)
  namespace <- .eolas_dataset_field(meta, "namespace", "")
  table     <- .eolas_dataset_field(meta, "table",
                                    .eolas_dataset_field(meta, "name", name))

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

  # ---- perform the request (streaming so we can show a progress bar) --------
  key <- eolas_get_key_internal()
  url <- paste0(base_url, "/v1/bulk/", namespace, "/", table)
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_error(is_error = \(r) FALSE)

  # Use req_perform_connection() for streaming so .eolas_stream_to_file()
  # can update the progress bar chunk-by-chunk.  Fall back to eolas_http_perform()
  # (which buffers the whole body) when a caller has mocked that function in tests.
  use_streaming <- .eolas_use_streaming()
  if (use_streaming) {
    conn_resp <- httr2::req_perform_connection(req)
    status    <- httr2::resp_status(conn_resp)
  } else {
    conn_resp <- eolas_http_perform(req)
    status    <- httr2::resp_status(conn_resp)
  }

  # ---- bulk-specific status handling ----------------------------------------
  if (status == 402L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% paste0(
      "Fresh bulk downloads are a Pro feature. Free accounts get the latest ",
      "monthly snapshot -- see https://eolas.fyi/pricing."
    )
    cli::cli_abort("Bulk upgrade required: {detail}", call. = FALSE)
  }

  if (status == 403L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      cli::cli_abort("Bulk licence restricted: {detail}", call. = FALSE)
    }
    # Key-auth 403 -- delegate to the standard status handler.
    eolas_check_status(conn_resp)
  }

  if (status == 503L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% paste0(
      "Monthly bulk snapshots are still rolling out for this dataset. ",
      "Try again after the 1st of next month, or upgrade to Pro for ",
      "on-demand current snapshots -- see https://eolas.fyi/pricing."
    )
    cli::cli_abort("Bulk not yet available: {detail}", call. = FALSE)
  }

  if (status != 200L) {
    eolas_check_status(conn_resp)
  }

  # ---- write or return ------------------------------------------------------
  if (is.null(path)) {
    # Bytes mode -- no progress bar (no file label to show).
    raw_bytes <- httr2::resp_body_raw(conn_resp)
    return(raw_bytes)
  }

  out_path <- normalizePath(path, mustWork = FALSE)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  show_bar    <- .eolas_resolve_progress(progress, "download")
  total_bytes <- .eolas_resp_content_length(conn_resp)
  label <- paste0("Downloading ", basename(out_path))

  rand_hex <- paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")
  tmp_path <- paste0(out_path, ".eolas-tmp-", rand_hex)

  bytes_dl <- tryCatch({
    if (use_streaming) {
      n <- .eolas_stream_to_file(conn_resp, tmp_path, total_bytes, label, show_bar)
      close(conn_resp)
      n
    } else {
      # Non-streaming (test mock) path -- body already buffered.
      raw_bytes <- httr2::resp_body_raw(conn_resp)
      writeBin(raw_bytes, tmp_path)
      length(raw_bytes)
    }
  }, error = function(e) {
    unlink(tmp_path)
    stop(e)
  })

  if (bytes_dl == 0L) {
    unlink(tmp_path)
    stop(
      "Bulk download for ", sQuote(name), " returned an empty body (0 bytes). ",
      "The snapshot may not exist for this dataset or format. ",
      "Use format = \"parquet\" for non-geo datasets.",
      call. = FALSE
    )
  }

  # Atomic rename onto the final path; fall back to copy+unlink cross-filesystem.
  ok <- file.rename(tmp_path, out_path)
  if (!ok) {
    file.copy(tmp_path, out_path, overwrite = TRUE)
    unlink(tmp_path)
  }

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
    cli::cli_abort("Bulk upgrade required: {detail}", call. = FALSE)
  }
  if (status == 403L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      cli::cli_abort("Bulk licence restricted: {detail}", call. = FALSE)
    }
    eolas_check_status(resp)
  }
  if (status == 503L) {
    body   <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    detail <- body$detail %||% "Monthly bulk snapshots are still rolling out."
    cli::cli_abort("Bulk not yet available: {detail}", call. = FALSE)
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
#' @param progress Control the download progress bar (`"download"` phase).
#'   See [eolas_get_local()] for the full `progress` selector vocabulary.
#'   When `status = "unchanged"` no download bar is shown; an informative
#'   cached-file message is printed instead.
#' @param force When `TRUE`, skip the sidecar "unchanged" fast path and
#'   re-download the bulk file even when the local snapshot id already matches
#'   the server (useful after corruption or to verify a fresh CDN copy).
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
                            progress  = NULL,
                            force     = FALSE,
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
  .eolas_apply_force(name, force, base_url = base_url)

  out_path    <- normalizePath(path, mustWork = FALSE)
  sidecar_path <- paste0(out_path, ".eolas-meta.json")

  # ---- read local sidecar ---------------------------------------------------
  prev <- if (file.exists(sidecar_path)) .read_sidecar(sidecar_path) else NULL

  # ---- resolve name -> namespace + table ------------------------------------
  meta      <- eolas_info(name, base_url = base_url)
  namespace <- .eolas_dataset_field(meta, "namespace", "")
  table     <- .eolas_dataset_field(meta, "table",
                                    .eolas_dataset_field(meta, "name", name))

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
  if (!isTRUE(force) &&
      !is.na(prev_sid) &&
      identical(prev_sid, current_sid) &&
      file.exists(out_path)) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_inform(c(
        "i" = "Using cached {.file {basename(out_path)}} (up to date)."
      ))
    }
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

  # Streaming path (real calls) vs buffered path (mocked tests).
  use_streaming <- .eolas_use_streaming()
  if (use_streaming) {
    conn_resp <- httr2::req_perform_connection(req)
    status    <- httr2::resp_status(conn_resp)
  } else {
    conn_resp <- eolas_http_perform(req)
    status    <- httr2::resp_status(conn_resp)
  }

  # ---- bulk-specific status handling (mirrors eolas_download_bulk) ----------
  if (status == 402L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% "Fresh bulk downloads are a Pro feature."
    cli::cli_abort("Bulk upgrade required: {detail}", call. = FALSE)
  }
  if (status == 403L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% ""
    if (nzchar(detail) && grepl("licence", detail, ignore.case = TRUE)) {
      cli::cli_abort("Bulk licence restricted: {detail}", call. = FALSE)
    }
    eolas_check_status(conn_resp)
  }
  if (status == 503L) {
    body_j <- tryCatch(httr2::resp_body_json(conn_resp), error = \(e) list())
    detail <- body_j$detail %||% "Monthly bulk snapshots are still rolling out."
    cli::cli_abort("Bulk not yet available: {detail}", call. = FALSE)
  }
  if (status != 200L) {
    eolas_check_status(conn_resp)
  }

  show_bar    <- .eolas_resolve_progress(progress, "download")
  total_bytes <- .eolas_resp_content_length(conn_resp)
  label <- paste0("Downloading ", basename(out_path))

  if (use_streaming) {
    bytes_dl <- .eolas_stream_to_file(conn_resp, tmp_path, total_bytes, label, show_bar)
    close(conn_resp)
  } else {
    raw_bytes <- httr2::resp_body_raw(conn_resp)
    bytes_dl  <- length(raw_bytes)
    writeBin(raw_bytes, tmp_path)
  }

  if (bytes_dl == 0L) {
    unlink(tmp_path)
    stop(
      "Bulk download for ", sQuote(name), " returned an empty body (0 bytes). ",
      "The snapshot may not exist for this dataset or format. ",
      "Use format = \"parquet\" for non-geo datasets.",
      call. = FALSE
    )
  }

  # Atomic rename onto the destination.
  ok <- file.rename(tmp_path, out_path)
  if (!ok) {
    # file.rename can fail across filesystems -- fall back to copy + unlink.
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
# eolas_cache_clear -- remove local bulk-cache files without re-downloading
# -------------------------------------------------------------------------

#' Clear cached state for a dataset (or the whole library)
#'
#' eolas caches at two levels: **session metadata** (`eolas_info()` per dataset,
#' used for routing and attached column glosses) and **on-disk bulk files**
#' (Parquet/GeoParquet in the library directory, with `.eolas-meta.json`
#' sidecars). This function clears one or both without contacting the API.
#'
#' Use [eolas_get()] or [eolas_get_local()] with `force = TRUE` to clear caches
#' and immediately re-fetch in one step.
#'
#' When `name` is set and `format = NULL`, removes on-disk files for **all**
#' bulk extensions (`.parquet`, `.csv.gz`, `.geo.parquet`) that exist for that
#' dataset. When `name = NULL` and `files = TRUE`, sweeps the entire library
#' directory for bulk data files and sidecars.
#'
#' @param name Dataset identifier, e.g. `"nz_parcels"`. `NULL` clears library-
#'   wide file caches (when `files = TRUE`) and/or all session metadata (when
#'   `meta = TRUE`).
#' @param cache_dir Library directory. `NULL` (default) uses the same
#'   precedence chain as [eolas_get_local()] (`EOLAS_LIBRARY`, config, fallback).
#' @param format `"parquet"`, `"csv_gz"`, or `"geoparquet"`. `NULL` (default)
#'   deletes any on-disk bulk variants for `name` (ignored when `name = NULL`).
#' @param files When `TRUE` (default), delete on-disk bulk data files and
#'   sidecars.
#' @param meta When `TRUE` (default), drop session-cached [eolas_info()] for
#'   `name` (or all datasets when `name = NULL`).
#' @param base_url Override the API base URL for metadata cache keys.
#' @return Invisibly a list with `files` (character vector of deleted paths)
#'   and `meta_cleared` (integer count of session cache entries removed).
#' @export
#' @examples
#' \dontrun{
#' # Free disk space without re-downloading
#' eolas_cache_clear("nz_parcels")
#'
#' # Metadata only (e.g. after a warehouse schema change)
#' eolas_cache_clear("nz_cpi", files = FALSE)
#'
#' # Nuclear option -- wipe library files + all session metadata
#' eolas_cache_clear(name = NULL)
#' }
#' @seealso [eolas_get()], [eolas_sync_bulk()], [eolas_get_local()], [eolas_library_status()]
eolas_cache_clear <- function(name = NULL,
                              cache_dir = NULL,
                              format    = NULL,
                              files     = TRUE,
                              meta      = TRUE,
                              base_url  = EOLAS_BASE_URL) {
  if (!is.null(name) && (!is.character(name) || length(name) != 1L || !nzchar(name))) {
    stop("`name` must be NULL or a non-empty string.", call. = FALSE)
  }

  meta_n <- if (isTRUE(meta)) {
    .eolas_meta_cache_clear(name, base_url = base_url)
  } else {
    0L
  }

  deleted <- character(0)
  if (isTRUE(files)) {
    if (is.null(cache_dir)) {
      cache_dir_expanded <- eolas_resolve_library_dir()
    } else {
      cache_dir_expanded <- path.expand(as.character(cache_dir))
    }
    cache_dir_abs <- tools::file_path_as_absolute(cache_dir_expanded)

    paths <- if (is.null(name)) {
      if (!dir.exists(cache_dir_abs)) {
        character(0)
      } else {
        all_files <- list.files(cache_dir_abs, full.names = TRUE)
        bulk_exts <- paste0("\\", gsub(".", "\\\\.", unname(.BULK_EXTENSIONS), fixed = TRUE), "$")
        is_bulk <- grepl(paste(bulk_exts, collapse = "|"), all_files)
        is_sidecar <- grepl("\\.eolas-meta\\.json$", all_files)
        all_files[is_bulk | is_sidecar]
      }
    } else if (is.null(format)) {
      unlist(lapply(.BULK_EXTENSIONS, function(ext) {
        p <- file.path(cache_dir_abs, paste0(name, ext))
        c(p, paste0(p, ".eolas-meta.json"))
      }), use.names = FALSE)
    } else {
      p <- .eolas_bulk_cache_paths(name, cache_dir_abs, format = format, base_url = base_url)
      c(p$data_path, p$sidecar_path)
    }

    for (p in unique(paths)) {
      if (file.exists(p)) {
        unlink(p)
        deleted <- c(deleted, normalizePath(p, mustWork = FALSE))
      }
    }
  }

  if ((length(deleted) || meta_n > 0L) && requireNamespace("cli", quietly = TRUE)) {
    label <- if (is.null(name)) "all datasets" else name
    parts <- character(0)
    if (length(deleted)) {
      parts <- c(parts, paste0(length(deleted), " file", if (length(deleted) != 1L) "s"))
    }
    if (meta_n > 0L) {
      parts <- c(parts, paste0(meta_n, " metadata entr", if (meta_n != 1L) "ies" else "y"))
    }
    cli::cli_inform(c(
      "i" = "Cleared {paste(parts, collapse = ' and ')} for {.field {label}}."
    ))
  }
  invisible(list(files = deleted, meta_cleared = meta_n))
}


# -------------------------------------------------------------------------
# eolas_get_local -- notebook-friendly whole-dataset convenience
# -------------------------------------------------------------------------

#' Download (or serve from cache) a whole dataset as a local data frame
#'
#' This is the recommended path for large or geospatial datasets in an
#' interactive R session or R Markdown notebook.  On the first call it fetches
#' the bulk file from CDN (milliseconds for monthly snapshots) and writes it to
#' `~/.cache/eolas/`.  On subsequent calls a lightweight HEAD request checks
#' whether the local file is still current; if so the cached copy is read
#' directly -- zero network I/O on the data payload.
#'
#' If you have been calling `eolas_get("nz_parcels")` on a 3-million-row
#' geospatial dataset and it takes 15+ minutes, use `eolas_get_local()`
#' instead -- it serves a pre-materialised GeoParquet from CDN, not a live
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
#' @param as_sf When `TRUE` and the file is GeoParquet, the function
#'   attempts to return an `sf` object via `sf::st_read()` or
#'   `sfarrow::st_read_parquet()`.  When `FALSE`, a plain data frame is
#'   returned regardless of geometry.  `NULL` (default) is treated as `TRUE`
#'   unless `as_arrow = TRUE`, in which case it is treated as `FALSE`.
#'   Cannot be combined with `as_arrow = TRUE` (stops with an error).
#' @param as_arrow When `TRUE`, skip all native geometry materialisation and
#'   return an `arrow::Table` directly.  Geometry stays as Arrow buffers
#'   (zero-copy) -- suitable for DuckDB / dplyr pipelines that work on a
#'   sample before converting to sf.  Works for geo and non-geo datasets.
#'   Cannot be combined with `as_sf = TRUE` (stops with an error).  Requires
#'   the `arrow` package: `install.packages("arrow")`.
#' @param meta When `TRUE` (default), attach dataset metadata from
#'   [eolas_info()] as object attributes. Pass `FALSE` to skip the extra
#'   round-trip.
#' @param progress Control progress feedback for the two bulk phases:
#'   **download** (streaming byte bar while fetching from CDN) and
#'   **read** (indeterminate spinner while Parquet/GeoParquet is
#'   materialised into a data frame or `sf` object). `NULL` (default)
#'   enables both in interactive sessions. `TRUE`/`FALSE` force both on/off.
#'   Use `"download"` or `"read"` to show only one phase. Suppressed by
#'   `EOLAS_NO_PROGRESS=1`. Cached snapshots skip the download bar and print
#'   an informative message instead.
#' @param force When `TRUE`, drop session [eolas_info()] cache and re-download
#'   the bulk file even when the sidecar says the snapshot is current. See
#'   [eolas_sync_bulk()] and [eolas_cache_clear()].
#' @param base_url Override the API base URL (useful for testing).
#' @param ... Reserved for future arguments; currently ignored.
#' @return A `data.frame`, `sf` object, or `arrow::Table`, depending on the
#'   dataset and the `as_sf` / `as_arrow` arguments.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#'
#' # 3-million-row geospatial dataset -- first call downloads GeoParquet from CDN;
#' # subsequent calls return in <1 s via sidecar check.
#' gdf <- eolas_get_local("nz_parcels")
#'
#' # Non-geo tabular dataset
#' df <- eolas_get_local("nz_cpi")
#'
#' # Explicit cache directory (overrides library config -- highest priority)
#' df <- eolas_get_local("nz_cpi", cache_dir = "/data/eolas-cache")
#'
#' # Force CSV format
#' df <- eolas_get_local("nz_cpi", format = "csv_gz")
#'
#' # Keep plain data.frame even for geo datasets
#' df <- eolas_get_local("nz_parcels", as_sf = FALSE)
#'
#' # Arrow table -- zero-copy, no sf allocation; suitable for DuckDB / dplyr
#' tbl <- eolas_get_local("nz_parcels", as_arrow = TRUE)
#' }
#' @seealso [eolas_sync_bulk()], `eolas_library_set()`, <https://docs.eolas.fyi/bulk-downloads/>
eolas_get_local <- function(name,
                             cache_dir = NULL,
                             format    = NULL,
                             freshness = "auto",
                             as_sf     = NULL,
                             as_arrow  = FALSE,
                             meta      = TRUE,
                             progress  = NULL,
                             force     = FALSE,
                             base_url  = EOLAS_BASE_URL,
                             ...) {

  # ---- argument validation --------------------------------------------------
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  freshness <- match.arg(freshness, .BULK_VALID_FRESHNESS)
  .eolas_apply_force(name, force, base_url = base_url)

  # ---- as_arrow / as_sf conflict guard -------------------------------------
  if (isTRUE(as_arrow) && isTRUE(as_sf)) {
    stop(
      "as_arrow = TRUE and as_sf = TRUE are mutually exclusive. ",
      "as_arrow returns an arrow::Table (no geometry materialisation); ",
      "as_sf materialises geometry as sf objects. Choose one.",
      call. = FALSE
    )
  }
  # Resolve as_sf NULL -> default TRUE unless as_arrow overrides.
  as_sf_resolved <- if (!is.null(as_sf)) as_sf else !isTRUE(as_arrow)

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

  meta_info <- .eolas_fetch_meta_info(name, base_url, meta)

  finish <- function(x) {
    if (isTRUE(as_arrow)) return(x)
    .eolas_finalize_dataset(x, name = name, meta_info = meta_info)
  }

  # ---- auto-detect format if not specified ----------------------------------
  if (is.null(format)) {
    fmt <- .eolas_detect_bulk_format(meta_info, name, base_url)
  } else {
    fmt <- match.arg(format, .BULK_VALID_FORMATS)
  }

  # ---- compute local file path ----------------------------------------------
  ext       <- .BULK_EXTENSIONS[[fmt]]            # e.g. ".parquet", ".csv.gz", ".geo.parquet"
  file_path <- file.path(cache_dir_abs, paste0(name, ext))

  # ---- sync (download if needed, HEAD check if cached) --------------------
  # Bulk-specific stop() errors (Bulk upgrade required / Bulk licence
  # restricted / Bulk not yet available) propagate unchanged -- their messages
  # already tell the user what to do.
  eolas_sync_bulk(name, path = file_path, format = fmt,
                  freshness = freshness, progress = progress, force = force,
                  base_url = base_url)

  show_read <- .eolas_resolve_progress(progress, "read")
  read_lbl  <- basename(file_path)
  read_prog <- function(expr) .eolas_with_read_progress(read_lbl, show_read, expr)

  # ---- read the local file into a data frame --------------------------------
  # as_arrow=TRUE: return arrow::Table directly, skipping all sf/WKB conversion.
  # Works for parquet, geoparquet, and csv_gz.
  if (isTRUE(as_arrow)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "The `arrow` package is required for as_arrow = TRUE. ",
        "Install with: install.packages(\"arrow\")",
        call. = FALSE
      )
    }
    if (fmt == "csv_gz") {
      # No native Arrow CSV-GZ reader -- read via utils::read.csv then convert.
      df_tmp <- read_prog(utils::read.csv(gzfile(file_path), stringsAsFactors = FALSE))
      return(arrow::as_arrow_table(df_tmp))
    } else {
      # parquet or geoparquet -- arrow reads both natively without sf overhead.
      return(read_prog(arrow::read_parquet(file_path, as_data_frame = FALSE)))
    }
  }

  if (fmt == "geoparquet" && isTRUE(as_sf_resolved)) {
    # Reader strategy (2026-05-27): try arrow + empty-aware WKB conversion
    # first (handles empty/null geometries which sfarrow aborts on); fall
    # back to sfarrow for older R installs without arrow; final fallback
    # is the WKT-string variant. See [[project_geoparquet_evolution]].
    primary_err <- NULL
    if (requireNamespace("arrow", quietly = TRUE) &&
        requireNamespace("sf",    quietly = TRUE)) {
      result <- tryCatch(
        read_prog(.eolas_arrow_wkb_to_sf(file_path)),
        error = function(e) {
          primary_err <<- e
          NULL
        }
      )
      if (!is.null(result)) return(finish(result))
    }

    # Last-resort sfarrow attempt (in case arrow isn't installed). sfarrow
    # is effectively dead upstream and known to fail on any GeoParquet with
    # zero-length WKB elements, but on Point-only datasets with no empties
    # it still works, so it's a reasonable safety net.
    if (requireNamespace("sfarrow", quietly = TRUE)) {
      sfarrow_err <- primary_err
      result <- tryCatch(
        read_prog(.eolas_sfarrow_read_parquet(file_path)),
        error = function(e) {
          sfarrow_err <<- e
          NULL
        }
      )
      if (!is.null(result)) return(finish(result))

      # sfarrow failed -- likely malformed GeoParquet metadata (e.g. empty
      # geometry_types array from an older S3 snapshot).  Fall back to the
      # plain .parquet variant which carries a geometry_wkt string column,
      # then promote it to sf via sf::st_as_sf().
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_warn(paste0(
          "Both arrow+WKB and sfarrow readers failed on the GeoParquet; ",
          "falling back to WKT string path. Returned data is correct; ",
          "this is just the slower read path."
        ))
      } else {
        warning(
          "Both arrow+WKB and sfarrow readers failed on the GeoParquet; ",
          "falling back to WKT string path. Returned data is correct; ",
          "this is just the slower read path.",
          call. = FALSE
        )
      }

      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop(
          conditionMessage(sfarrow_err),
          "\n\nWorkaround: install the `arrow` package ",
          "(install.packages(\"arrow\")) to enable the geometry_wkt fallback.",
          call. = FALSE
        )
      }
      if (!requireNamespace("sf", quietly = TRUE)) {
        stop(
          conditionMessage(sfarrow_err),
          "\n\nWorkaround: install the `sf` package ",
          "(install.packages(\"sf\")) to enable the geometry_wkt fallback.",
          call. = FALSE
        )
      }

      # Download (or serve from cache) the plain .parquet variant.
      parquet_path <- file.path(
        dirname(file_path),
        paste0(name, ".parquet")
      )
      fallback_err <- tryCatch({
        eolas_sync_bulk(
          name,
          path      = parquet_path,
          format    = "parquet",
          freshness = freshness,
          progress  = progress,
          base_url  = base_url
        )
        NULL
      }, error = function(e) e)

      if (!is.null(fallback_err)) {
        stop(
          conditionMessage(sfarrow_err),
          "\n\nThe geometry_wkt fallback also failed: ",
          conditionMessage(fallback_err),
          call. = FALSE
        )
      }

      df <- tryCatch(
        read_prog(as.data.frame(arrow::read_parquet(parquet_path))),
        error = function(e) {
          stop(
            conditionMessage(sfarrow_err),
            "\n\nThe geometry_wkt fallback also failed while reading the plain ",
            "Parquet: ", conditionMessage(e),
            call. = FALSE
          )
        }
      )

      if (!"geometry_wkt" %in% names(df)) {
        stop(
          conditionMessage(sfarrow_err),
          "\n\nThe geometry_wkt fallback failed: the plain Parquet file has no ",
          "'geometry_wkt' column. Contact support@eolas.fyi.",
          call. = FALSE
        )
      }

      # Use the same defensive .eolas_to_sf() helper used by the live-API path:
      # it handles blank/null/malformed WKT rows without aborting (they become
      # EMPTY geometry), which is important for large datasets like nz_parcels.
      sf_obj <- tryCatch(
        .eolas_to_sf(df, force = TRUE),
        error = function(e) {
          stop(
            conditionMessage(sfarrow_err),
            "\n\nThe geometry_wkt fallback also failed during sf conversion: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      return(finish(sf_obj))
    }
    if (requireNamespace("sf", quietly = TRUE)) {
      return(finish(read_prog(sf::st_read(file_path, quiet = TRUE))))
    }
    # Neither sf nor sfarrow available -- fall through to plain read below.
    cli::cli_alert_info(c(
      "{.pkg sf} or {.pkg sfarrow} needed to return a GeoParquet as an {.cls sf} object.",
      "i" = "Install with {.run install.packages(\"sf\")}. Returning plain {.cls data.frame}."
    ))
  }

  # Plain read paths.
  if (fmt == "csv_gz") {
    return(finish(read_prog(utils::read.csv(gzfile(file_path), stringsAsFactors = FALSE))))
  }

  # parquet or geoparquet-without-sf. arrow is a hard dependency (it's on the
  # default bulk/large-dataset read path), but check_installed still gives a
  # clean, install-offering message if the user's arrow build is broken.
  rlang::check_installed("arrow", reason = "to read Parquet data from eolas")
  finish(read_prog(as.data.frame(arrow::read_parquet(file_path))))
}
