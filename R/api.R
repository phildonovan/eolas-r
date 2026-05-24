#' List available datasets
#'
#' Returns a tibble (or data frame) with one row per dataset, including name,
#' title, source, namespace, and description.
#'
#' @param source Optional source filter, e.g. `"Stats NZ"` or `"OECD"`.
#'   Use [eolas_list_statsnz()], [eolas_list_oecd()] etc. as convenient shortcuts.
#' @param base_url Override the API base URL (useful for testing).
#' @return A tibble (if the `tibble` package is installed) or a `data.frame`.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' eolas_list()
#' eolas_list("Stats NZ")
#' }
eolas_list <- function(source = NULL, base_url = EOLAS_BASE_URL) {
  resp <- eolas_http_get("/v1/datasets", base_url = base_url)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df   <- as.data.frame(body$datasets %||% body)

  if (!is.null(source)) {
    df <- df[!is.na(df$source) & df$source == source, ]
    rownames(df) <- NULL
  }

  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(df) else df
}


#' Get metadata for a single dataset
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param base_url Override the API base URL (useful for testing).
#' @return A named list with dataset metadata.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' eolas_info("nz_cpi")
#' }
eolas_info <- function(name, base_url = EOLAS_BASE_URL) {
  resp <- eolas_http_get(paste0("/v1/datasets/", name), base_url = base_url)
  httr2::resp_body_json(resp)
}


# Per-session set: tracks which dataset names we have already emitted the
# auto-routing message for, so we don't spam on repeated calls.
.eolas_auto_route_notified <- new.env(parent = emptyenv())

# Row-count threshold above which a bulk-eligible dataset is auto-routed through
# the cache+sync path instead of a live Iceberg scan.
.EOLAS_AUTO_ROUTE_ROW_THRESHOLD <- 100000L


# Internal: one-time nudge for users on the slower JSON path. Pushy (every
# JSON-path user is told the exact fix + the measured win) but never aborts —
# `arrow` stays in Suggests so install never breaks on a constrained box.
.eolas_nag_arrow_once <- function() {
  if (isTRUE(.eolas_runtime$arrow_nagged)) return(invisible())
  .eolas_runtime$arrow_nagged <- TRUE
  message(
    "eolas: using the slower JSON transport. Install the 'arrow' package for ",
    "much faster downloads (measured ~5x faster end-to-end, ~82x faster parse ",
    "on large datasets):\n  install.packages(\"arrow\")"
  )
}

# Internal: fetch dataset rows as a data.frame. Negotiates Arrow IPC over the
# wire (typed, columnar — far faster than JSON for large pulls), transparently
# falling back to JSON for older servers, a missing `arrow` package, or any
# parse issue. The returned data.frame is identical either way.
.eolas_fetch_df <- function(name, params, base_url) {
  path <- paste0("/v1/datasets/", name, "/data")

  if (requireNamespace("arrow", quietly = TRUE)) {
    if (!isFALSE(.eolas_runtime$arrow_supported)) {
      resp <- tryCatch(
        do.call(eolas_http_get,
          c(list(path, base_url = base_url, format = "arrow"), params)),
        error = function(e) NULL
      )
      ctype <- if (is.null(resp)) "" else (httr2::resp_content_type(resp) %||% "")
      if (!is.null(resp) && grepl("arrow", ctype, fixed = TRUE)) {
        .eolas_runtime$arrow_supported <- TRUE
        tbl <- arrow::read_ipc_stream(httr2::resp_body_raw(resp))
        return(as.data.frame(tbl))
      }
      # Old server ignored format=arrow — remember so we don't pay the failed
      # round-trip on every future call this session.
      if (!is.null(resp)) .eolas_runtime$arrow_supported <- FALSE
    }
  } else {
    .eolas_nag_arrow_once()
  }

  resp <- do.call(eolas_http_get,
    c(list(path, base_url = base_url), params))
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  as.data.frame(body$data %||% body)
}

#' Fetch dataset rows
#'
#' The generic workhorse — use [eolas_get_statsnz()], [eolas_get_oecd()] etc. for
#' source-tagged results and a nicer print output.
#'
#' The `mode` parameter controls which data path is used:
#'
#' - `"auto"` (default): smart-routes based on dataset metadata.
#'   If any slice argument (`start`, `end`, `limit`) is set the live API is
#'   always used.  Otherwise `eolas_info(name)` is called and the result
#'   routed through [eolas_get_local()] (cache+sync) when the dataset is
#'   bulk-eligible and large (>100k rows) or geospatial.  OECD and other
#'   licence-restricted datasets always fall through to live regardless of size.
#' - `"live"`: always use the live API endpoint, bypassing the cache.
#' - `"cached"`: always use the cache+sync path (equivalent to calling
#'   [eolas_get_local()]).  `start`, `end`, and `limit` are ignored.
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end   ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @param limit Max rows to return. Default `NULL` requests the full dataset
#'   (server enforces a 50,000-row cap on Free/Starter plans; Pro is unlimited).
#'   Pass an integer to request fewer rows.
#' @param as_sf Convert geospatial datasets to an `sf` object (CRS = WGS84).
#'   `NULL` (default) auto-converts when the dataset has a `geometry_wkt`
#'   column AND the `sf` package is installed. `TRUE` forces conversion (errors
#'   if `sf` is missing). `FALSE` keeps the raw WKT string column.
#'   Install with `install.packages("sf")`.
#' @param mode `"auto"` (default), `"live"`, or `"cached"`. Controls
#'   smart-routing behaviour; see Description above.
#' @param base_url Override the API base URL (useful for testing).
#' @return A `eolas_dataset` data frame with `date` coerced to `Date`, or an
#'   `sf` object when geometry is present and conversion is enabled.  When
#'   routed through the cache+sync path (`"auto"` for large/geo datasets, or
#'   `"cached"`), the return value is a plain `data.frame` or `sf` object as
#'   returned by [eolas_get_local()].
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get("nz_cpi", start = "2020-01-01")
#' library(ggplot2)
#' ggplot(df, aes(date, value)) + geom_line()
#'
#' # Smart-routing: nz_parcels auto-routes to cache+sync in seconds
#' gdf <- eolas_get("nz_parcels")
#'
#' # Force live even for large datasets
#' gdf <- eolas_get("nz_parcels", mode = "live")
#'
#' # Force cache+sync explicitly (same as eolas_get_local)
#' gdf <- eolas_get("nz_parcels", mode = "cached")
#' }
eolas_get <- function(name, start = NULL, end = NULL, limit = NULL,
                   as_sf = NULL, mode = "auto", base_url = EOLAS_BASE_URL) {

  mode <- match.arg(mode, c("auto", "live", "cached"))

  # ---- mode="cached": delegate entirely to the cache+sync path ---------------
  if (mode == "cached") {
    as_sf_local <- if (is.null(as_sf)) TRUE else isTRUE(as_sf)
    return(eolas_get_local(name, as_sf = as_sf_local, base_url = base_url))
  }

  # ---- mode="auto": decide based on slice args + dataset metadata ------------
  if (mode == "auto") {
    has_slice <- !is.null(start) || !is.null(end) || !is.null(limit)
    if (!has_slice) {
      meta <- tryCatch(
        eolas_info(name, base_url = base_url),
        error = function(e) list()
      )

      bulk_export_class <- meta$bulk_export_class %||% "none"
      bulk_ok <- nzchar(bulk_export_class) && bulk_export_class != "none"

      is_geo <- !is.null(meta$geometry_type) || !is.null(meta$geometry_wkt) ||
                isTRUE(meta$has_geometry)
      row_count <- as.integer(meta$row_count_at_last_refresh %||% 0L)
      is_large  <- row_count > .EOLAS_AUTO_ROUTE_ROW_THRESHOLD

      if (bulk_ok && (is_geo || is_large)) {
        # One-time per-dataset message so notebook users see what's happening.
        name_str <- as.character(name)
        if (!exists(name_str, envir = .eolas_auto_route_notified, inherits = FALSE)) {
          assign(name_str, TRUE, envir = .eolas_auto_route_notified)
          message(
            "eolas: auto-routing '", name_str, "' through cache+sync (large/geo dataset).\n",
            "       Cache lives at ~/.cache/eolas/. Use mode='live' to override."
          )
        }
        as_sf_local <- if (is.null(as_sf)) TRUE else isTRUE(as_sf)
        return(eolas_get_local(name, as_sf = as_sf_local, base_url = base_url))
      }
      # Fall through to the live path.
    }
  }

  # ---- live path (mode="live", or auto fell through) -------------------------
  # Server-side: limit=0 means "as many rows as your plan allows" (50,000 cap on
  # Free/Starter, unlimited on Pro). NULL on the R side maps to limit=0.
  params <- list()
  if (!is.null(start)) params$start <- start
  if (!is.null(end))   params$end   <- end
  params$limit <- if (is.null(limit)) 0L else as.integer(limit)

  df <- .eolas_fetch_df(name, params, base_url)

  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
    # API streams from Iceberg in file order, not chronological — sort here so
    # callers can `ggplot(df, aes(date, value)) + geom_line()` without zigzag.
    df <- df[order(df$date), , drop = FALSE]
    rownames(df) <- NULL
  }

  result <- new_eolas_dataset(df, name = name)

  # Optional sf conversion. as_sf=NULL auto-converts when (a) geometry_wkt is
  # present AND (b) the sf package is installed. as_sf=TRUE forces and errors
  # if sf is missing; as_sf=FALSE keeps the raw WKT string column.
  if (!isFALSE(as_sf) && "geometry_wkt" %in% names(result)) {
    result <- .eolas_to_sf(result, force = isTRUE(as_sf))
  }

  result
}


# Internal: convert a data frame with a geometry_wkt column to an sf object
# (CRS = WGS84 / EPSG:4326). Returns the original df if sf isn't installed and
# `force` is FALSE; errors if `force` is TRUE.
.eolas_to_sf <- function(df, force = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    if (force) {
      stop("The 'sf' package is required to return geospatial datasets as sf ",
           "objects. Install with: install.packages('sf')", call. = FALSE)
    }
    return(df)
  }
  # Preserve eolas_dataset-style metadata so source/name attrs survive the conversion
  vs_name   <- attr(df, "eolas_name")
  vs_source <- attr(df, "eolas_source")
  # Convert to plain data.frame first — sf::st_as_sf doesn't reliably drop the
  # WKT column when called on a class-extended data frame (e.g. eolas_dataset).
  plain <- as.data.frame(df)

  # Some NZ geospatial datasets legitimately contain rows with no geometry —
  # e.g. non-digitised "oceanic" / "area outside region" meshblocks. A single
  # such row makes sf::st_as_sf(wkt=) abort the whole conversion with
  # "OGR: Unsupported geometry type". Parse defensively: blank/NA/sentinel
  # values and any individually-unparseable WKT become EMPTY geometry, every
  # attribute row is preserved, and the caller gets a warning with the count.
  raw   <- as.character(plain[["geometry_wkt"]])
  trimmed <- trimws(raw)
  blank <- is.na(raw) | !nzchar(trimmed) |
           toupper(trimmed) %in% c("NA", "NONE", "NULL", "NAN")

  # Cheap shape screen: a WKT value must start with an OGC/ISO geometry
  # keyword (optionally SRID-prefixed). Values that fail this are not WKT at
  # all (sentinels, error text) — classify them without handing GDAL a
  # garbage string, which both avoids GDAL's stderr chatter and is faster.
  wkt_kw <- paste0(
    "^(SRID=\\d+\\s*;\\s*)?(POINT|LINESTRING|POLYGON|MULTIPOINT|",
    "MULTILINESTRING|MULTIPOLYGON|GEOMETRYCOLLECTION|CIRCULARSTRING|",
    "COMPOUNDCURVE|CURVEPOLYGON|MULTICURVE|MULTISURFACE|",
    "POLYHEDRALSURFACE|TIN|TRIANGLE)\\b")
  not_wkt <- !blank & !grepl(wkt_kw, toupper(trimmed))

  geoms <- vector("list", length(raw))
  bad   <- not_wkt
  idx   <- which(!blank & !not_wkt)

  # Fast path: vectorised parse of the plausibly-WKT rows. Succeeds for the
  # common case (only problem rows were blank/non-WKT), so we pay the per-row
  # cost only when there is genuinely malformed WKT among them (e.g. a
  # server-side truncated polygon).
  parsed <- if (length(idx))
    tryCatch(sf::st_as_sfc(raw[idx], crs = 4326), error = function(e) NULL)
  else
    sf::st_sfc(crs = 4326)

  if (length(idx) && is.null(parsed)) {
    for (j in seq_along(idx)) {
      i <- idx[j]
      g <- tryCatch(sf::st_as_sfc(raw[i], crs = 4326),
                     error = function(e) NULL)
      if (is.null(g)) bad[i] <- TRUE else geoms[[i]] <- g[[1]]
    }
  } else if (length(idx)) {
    for (j in seq_along(idx)) geoms[[idx[j]]] <- parsed[[j]]
  }

  empty_i <- which(blank | bad)
  if (length(empty_i)) {
    eg <- sf::st_geometrycollection()  # EMPTY; type-agnostic placeholder
    for (i in empty_i) geoms[[i]] <- eg
  }

  n_blank <- sum(blank)
  n_bad   <- sum(bad)
  if (n_blank || n_bad) {
    warning(sprintf(
      paste0("eolas: %d of %d row(s) had no usable geometry ",
             "(%d empty/null, %d unparseable WKT) and were returned with ",
             "EMPTY geometry. Filter with !sf::st_is_empty(x) if needed."),
      n_blank + n_bad, length(raw), n_blank, n_bad), call. = FALSE)
  }

  plain[["geometry_wkt"]] <- NULL
  # Geometry column is named "geometry" for consistency with the sf ecosystem.
  result <- sf::st_sf(plain, geometry = sf::st_sfc(geoms, crs = 4326))
  if (!is.null(vs_name))   attr(result, "eolas_name")   <- vs_name
  if (!is.null(vs_source)) attr(result, "eolas_source") <- vs_source
  result
}
