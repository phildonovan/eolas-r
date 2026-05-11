# Source-specific get and list functions.
# Each vs_get_<source>() is a named wrapper over eolas_get() that tags the
# result with the source label, used by the eolas_dataset print method.

.eolas_get_source <- function(name, source, start = NULL, end = NULL,
                            limit = NULL, as_sf = NULL,
                            base_url = EOLAS_BASE_URL) {
  df <- eolas_get(name, start = start, end = end, limit = limit,
               as_sf = as_sf, base_url = base_url)
  attr(df, "eolas_source") <- source
  df
}

.eolas_list_source <- function(source) {
  df <- eolas_list()
  df[!is.na(df$source) & df$source == source, ]
}


# ---------------------------------------------------------------------------
# Stats NZ
# ---------------------------------------------------------------------------

#' Fetch a Stats NZ series
#'
#' A named wrapper over [eolas_get()] that tags the result with the
#' "Stats NZ" source label.
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
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is present
#'   and conversion is enabled.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get_statsnz("nz_cpi", start = "2015-01-01")
#' library(ggplot2)
#' ggplot(df, aes(date, value)) + geom_line()
#' }
eolas_get_statsnz <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Stats NZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Stats NZ series
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_statsnz <- function() .eolas_list_source("Stats NZ")


# ---------------------------------------------------------------------------
# OECD
# ---------------------------------------------------------------------------

#' Fetch an OECD series
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_oecd <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "OECD", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all OECD series
#' @export
eolas_list_oecd <- function() .eolas_list_source("OECD")


# ---------------------------------------------------------------------------
# RBNZ
# ---------------------------------------------------------------------------

#' Fetch an RBNZ series
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_rbnz <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "RBNZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all RBNZ series
#' @export
eolas_list_rbnz <- function() .eolas_list_source("RBNZ")


# ---------------------------------------------------------------------------
# NZ Treasury
# ---------------------------------------------------------------------------

#' Fetch a NZ Treasury series
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_treasury <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "NZ Treasury", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all NZ Treasury series
#' @export
eolas_list_treasury <- function() .eolas_list_source("NZ Treasury")


# ---------------------------------------------------------------------------
# LINZ
# ---------------------------------------------------------------------------

#' Fetch a LINZ series
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_linz <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "LINZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all LINZ series
#' @export
eolas_list_linz <- function() .eolas_list_source("LINZ")


# ---------------------------------------------------------------------------
# Stats NZ Geospatial (server-side source label collapsed to "Stats NZ"; we
# keep these helpers for discoverability and filter by namespace instead).
# ---------------------------------------------------------------------------

#' Fetch a Stats NZ geospatial dataset (boundaries, census meshblocks, etc.).
#'
#' The server returns `source = "Stats NZ"` for both SDMX time series and
#' Datafinder geospatial datasets — the eolas_dataset attribute reflects that.
#' This helper exists as a discoverability shortcut, not a separate source.
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_statsnz_geo <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Stats NZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List Stats NZ geospatial datasets only (filtered by namespace).
#'
#' Filters on `namespace == "statsnz_geo"` rather than the source label, because
#' the source label "Stats NZ" is now shared with the SDMX time-series datasets.
#' @export
eolas_list_statsnz_geo <- function() {
  df <- eolas_list()
  df <- df[!is.na(df$namespace) & df$namespace == "statsnz_geo", ]
  rownames(df) <- NULL
  df
}


# ---------------------------------------------------------------------------
# MBIE
# ---------------------------------------------------------------------------

#' Fetch an MBIE dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_mbie <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "MBIE", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all MBIE datasets
#' @export
eolas_list_mbie <- function() .eolas_list_source("MBIE")


# ---------------------------------------------------------------------------
# Waka Kotahi (NZTA)
# ---------------------------------------------------------------------------

#' Fetch a Waka Kotahi (NZTA) dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_nzta <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Waka Kotahi", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Waka Kotahi datasets
#' @export
eolas_list_nzta <- function() .eolas_list_source("Waka Kotahi")


# ---------------------------------------------------------------------------
# MSD
# ---------------------------------------------------------------------------

#' Fetch an MSD dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_msd <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "MSD", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all MSD datasets
#' @export
eolas_list_msd <- function() .eolas_list_source("MSD")


# ---------------------------------------------------------------------------
# NZ Police / MoJ
# ---------------------------------------------------------------------------

#' Fetch an NZ Police / MoJ dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_police <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "NZ Police / MoJ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all NZ Police / MoJ datasets
#' @export
eolas_list_police <- function() .eolas_list_source("NZ Police / MoJ")


# ---------------------------------------------------------------------------
# ACC
# ---------------------------------------------------------------------------

#' Fetch an ACC dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_acc <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "ACC", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all ACC datasets
#' @export
eolas_list_acc <- function() .eolas_list_source("ACC")


# ---------------------------------------------------------------------------
# Education Counts
# ---------------------------------------------------------------------------

#' Fetch an Education Counts dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_edcounts <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Education Counts", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Education Counts datasets
#' @export
eolas_list_edcounts <- function() .eolas_list_source("Education Counts")


# ---------------------------------------------------------------------------
# WorkSafe NZ
# ---------------------------------------------------------------------------

#' Fetch a WorkSafe NZ dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_worksafe <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "WorkSafe NZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all WorkSafe NZ datasets
#' @export
eolas_list_worksafe <- function() .eolas_list_source("WorkSafe NZ")
