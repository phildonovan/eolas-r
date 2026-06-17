#' List available datasets
#'
#' Returns a tibble (or data frame) with one row per dataset, including name,
#' title, source, namespace, and description.
#'
#' @param source Optional source filter, e.g. `"Stats NZ"` or `"OECD"`.
#'   Use [eolas_list_statsnz()], [eolas_list_oecd()] etc. as convenient shortcuts.
#' @param base_url Override the API base URL (useful for testing).
#' @return A tibble.
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

  tibble::as_tibble(df)
}


.eolas_search_aliases <- list(
  hlfs    = c("hlfs", "household labour", "labour force survey", "labour force",
              "labour market", "unemployment rate"),
  ocr     = c("ocr", "official cash rate", "cash rate"),
  cpi     = c("cpi", "consumer price", "inflation", "prices"),
  kapiti  = c("kapiti", "kcdc_", "kapiti coast"),
  porirua = c("porirua", "pcc_")
)

.eolas_search_name_only <- c("hlfs", "kapiti", "porirua")

.eolas_search_rank <- list(
  cpi  = c(rbnz_m1_prices = 0L, rbnz_m1_prices_longrun = 1L, nz_cpi = 2L),
  hlfs = c(nz_unemployment = 0L, rbnz_m9_labour_market = 1L,
           poppr_lab_national = 2L, poppr_lab_national_chars = 3L)
)

.eolas_cpi_guidance <- function() {
  paste0(
    "nz_cpi is OECD annual % change (quarterly). ",
    "For CPI index levels use rbnz_m1_prices (RBNZ, quarterly index)."
  )
}

.eolas_search_terms <- function(query) {
  q <- tolower(trimws(query))
  if (!nzchar(q)) return(character(0))
  aliases <- .eolas_search_aliases[[q]]
  if (!is.null(aliases)) aliases else q
}

.eolas_search_mask <- function(df, needles, search_description = TRUE) {
  if (!nrow(df) || !length(needles)) return(rep(FALSE, nrow(df)))
  cols <- intersect(c("name", "title"), names(df))
  if (search_description) cols <- c(cols, intersect("description", names(df)))
  if (!length(cols)) return(rep(FALSE, nrow(df)))
  mask <- rep(FALSE, nrow(df))
  for (col in cols) {
    hay <- tolower(ifelse(is.na(df[[col]]), "", as.character(df[[col]])))
    for (needle in tolower(needles)) {
      mask <- mask | grepl(needle, hay, fixed = TRUE)
    }
  }
  mask
}


#' Search datasets by name, title, or description
#'
#' Substring search over the dataset catalog. Common analyst tokens are
#' expanded — e.g. `"HLFS"` also matches labour-force and unemployment
#' datasets; `"OCR"` matches official cash rate series.
#'
#' @param query Search string (case-insensitive).
#' @param source Optional source filter, e.g. `"RBNZ"`.
#' @param base_url Override the API base URL (useful for testing).
#' @return A tibble of matching datasets (same columns as [eolas_list()]).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' eolas_search("HLFS")
#' eolas_search("OCR", source = "RBNZ")
#' }
eolas_search <- function(query, source = NULL, base_url = EOLAS_BASE_URL) {
  df <- eolas_list(source = source, base_url = base_url)
  needles <- .eolas_search_terms(query)
  rank_key <- tolower(trimws(query))
  if (!length(needles)) {
    return(tibble::as_tibble(df[0, , drop = FALSE]))
  }
  name_only <- rank_key %in% .eolas_search_name_only
  out <- tibble::as_tibble(df[.eolas_search_mask(df, needles, search_description = !name_only), , drop = FALSE])
  if (nrow(out) && "name" %in% names(out) && rank_key %in% names(.eolas_search_rank)) {
    priority <- .eolas_search_rank[[rank_key]]
    ord <- order(ifelse(out$name %in% names(priority), priority[out$name], 99L))
    out <- out[ord, , drop = FALSE]
  }
  if (tolower(trimws(query)) %in% c("cpi", "consumer price", "consumer price index", "inflation")) {
    cli::cli_inform(.eolas_cpi_guidance())
  }
  out
}


#' Get metadata for a single dataset
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param base_url Override the API base URL (useful for testing).
#' @return A one-row tibble with dataset metadata.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' eolas_info("nz_cpi")
#' }
eolas_info <- function(name, base_url = EOLAS_BASE_URL) {
  resp <- eolas_http_get(paste0("/v1/datasets/", name), base_url = base_url)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  .eolas_parse_info_response(body)
}


# Internal: one-time nudge for users on the slower JSON path. Pushy (every
# JSON-path user is told the exact fix + the measured win) but never aborts —
# `arrow` stays in Suggests so install never breaks on a constrained box.
.eolas_nag_arrow_once <- function() {
  if (isTRUE(.eolas_runtime$arrow_nagged)) return(invisible())
  .eolas_runtime$arrow_nagged <- TRUE
  cli::cli_alert_info(c(
    "Using the slower JSON transport.",
    "i" = "Install {.pkg arrow} for {.strong much faster} downloads (~5× end-to-end, ~82× parse on large datasets): {.run install.packages(\"arrow\")}"
  ))
}

# Internal: fetch dataset rows as a data.frame. Negotiates Arrow IPC over the
# wire (typed, columnar — far faster than JSON for large pulls), transparently
# falling back to JSON for older servers, a missing `arrow` package, or any
# parse issue. The returned data.frame is identical either way.
.eolas_fetch_df <- function(name, params, base_url, envelope = FALSE) {
  path <- paste0("/v1/datasets/", name, "/data")
  if (isTRUE(envelope)) params$envelope <- 1L

  if (requireNamespace("arrow", quietly = TRUE) && !isTRUE(envelope)) {
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
        return(list(
          df = as.data.frame(tbl),
          resp = resp,
          data_sources = NULL
        ))
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
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  rows <- body$data %||% body
  sources <- if (is.list(body) && !is.null(body$data_sources)) body$data_sources else NULL
  list(
    df = as.data.frame(rows, stringsAsFactors = FALSE),
    resp = resp,
    data_sources = sources
  )
}

#' Fetch dataset rows
#'
#' The generic workhorse — use [eolas_get_statsnz()], [eolas_get_oecd()] etc. for
#' source-tagged results and a nicer print output.
#'
#' Hits the live `/v1/datasets/{name}/data` endpoint.  Use [eolas_download_bulk()]
#' or [eolas_sync_bulk()] for large datasets or whole-dataset pulls.
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end   ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @param limit Max rows to return. Default `NULL` requests the full dataset
#'   (server enforces a 50,000-row cap on Free/Starter plans; Pro is unlimited).
#'   When set and a `date` column is present, returns the **most recent** N rows.
#' @param as_sf Convert geospatial datasets to an `sf` object (CRS = WGS84).
#'   `NULL` (default) auto-converts when the dataset has a `geometry_wkt`
#'   column AND the `sf` package is installed. `TRUE` forces conversion (errors
#'   if `sf` is missing). `FALSE` keeps the raw WKT string column.
#'   Install with `install.packages("sf")`. Cannot be combined with
#'   `as_arrow = TRUE`.
#' @param as_arrow When `TRUE`, return an `arrow::Table` instead of a
#'   `data.frame` or `sf` object.  Geometry stays as Arrow buffers
#'   (zero-copy, no sf allocation) — suitable for DuckDB / dplyr pipelines.
#'   Works on every dataset. Cannot be combined with `as_sf = TRUE` (stops
#'   with an error). Requires the `arrow` package: `install.packages("arrow")`.
#' @param meta When `TRUE` (default), fetch dataset metadata from
#'   `GET /v1/datasets/{name}` once per session (cached) and attach it as
#'   `eolas_meta` / `eolas_columns` attributes. Pass `FALSE` to skip the extra
#'   round-trip. See [eolas_meta()] and [eolas_column_label()].
#' @param envelope When `TRUE`, request `?envelope=1` (JSON only) and attach
#'   the `data_sources` licence block. Response `X-Eolas-*` headers are merged
#'   into metadata.
#' @param base_url Override the API base URL (useful for testing).
#' @return A `eolas_dataset` tibble with `date` coerced to `Date`, or an
#'   `sf` object when geometry is present and conversion is enabled. Table and
#'   column metadata are attached as attributes (not printed by default).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get("nz_cpi", start = "2020-01-01")
#' library(ggplot2)
#' ggplot(df, aes(date, value)) + geom_line()
#' }
eolas_get <- function(name, start = NULL, end = NULL, limit = NULL,
                   as_sf = NULL, as_arrow = FALSE, meta = TRUE,
                   envelope = FALSE, base_url = EOLAS_BASE_URL) {

  # ---- as_arrow / as_sf conflict guard ----------------------------------------
  if (isTRUE(as_arrow) && isTRUE(as_sf)) {
    stop(
      "as_arrow = TRUE and as_sf = TRUE are mutually exclusive. ",
      "as_arrow returns an arrow::Table (no geometry materialisation); ",
      "as_sf materialises geometry as sf objects. Choose one.",
      call. = FALSE
    )
  }
  if (isTRUE(envelope) && isTRUE(as_arrow)) {
    stop(
      "envelope = TRUE requires as_arrow = FALSE (JSON envelope only).",
      call. = FALSE
    )
  }

  # ---- live path ---------------------------------------------------------------
  limits <- .eolas_resolve_fetch_limit(limit)
  params <- list()
  if (!is.null(start)) params$start <- start
  if (!is.null(end))   params$end   <- end
  params$limit <- limits$fetch

  meta_info <- .eolas_fetch_meta_info(name, base_url, meta)

  fetched <- .eolas_fetch_df(name, params, base_url, envelope = envelope)
  df <- fetched$df
  if (!is.null(fetched$resp)) {
    meta_info <- .eolas_merge_provenance(meta_info, .eolas_provenance_from_headers(fetched$resp))
  }
  if (!is.null(fetched$data_sources) && is.data.frame(meta_info) && nrow(meta_info) >= 1L) {
    meta_info$data_sources <- list(fetched$data_sources)
  }

  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
    # API streams from Iceberg in file order, not chronological — sort here so
    # callers can `ggplot(df, aes(date, value)) + geom_line()` without zigzag.
    df <- df[order(df$date), , drop = FALSE]
    rownames(df) <- NULL
  }
  df <- .eolas_apply_row_limit(df, limits$user)

  # as_arrow on the live path: convert the data.frame to an arrow::Table,
  # avoiding any sf/shapely allocation. geometry_wkt stays as a character column.
  if (isTRUE(as_arrow)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "The `arrow` package is required for as_arrow = TRUE. ",
        "Install with: install.packages(\"arrow\")",
        call. = FALSE
      )
    }
    return(arrow::as_arrow_table(df))
  }

  result <- new_eolas_dataset(df, name = name, meta_info = meta_info)

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
  # Preserve eolas_dataset-style metadata so attrs survive the conversion
  vs_name    <- attr(df, "eolas_name")
  vs_source  <- attr(df, "eolas_source")
  vs_meta    <- attr(df, "eolas_meta")
  vs_columns <- attr(df, "eolas_columns")
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
    n_missing <- n_blank + n_bad
    n_total   <- length(raw)
    cli::cli_warn(c(
      "{.val {n_missing}} of {.val {n_total}} row(s) had no usable geometry ({.val {n_blank}} empty/null, {.val {n_bad}} unparseable WKT) and were returned with {.strong EMPTY} geometry.",
      "i" = "Filter with {.code !sf::st_is_empty(x)} if needed."
    ))
  }

  plain[["geometry_wkt"]] <- NULL
  # Geometry column is named "geometry" for consistency with the sf ecosystem.
  result <- sf::st_sf(plain, geometry = sf::st_sfc(geoms, crs = 4326))
  if (!is.null(vs_name))    attr(result, "eolas_name")    <- vs_name
  if (!is.null(vs_source))  attr(result, "eolas_source")  <- vs_source
  if (!is.null(vs_meta))    attr(result, "eolas_meta")    <- vs_meta
  if (!is.null(vs_columns)) attr(result, "eolas_columns") <- vs_columns
  result
}
