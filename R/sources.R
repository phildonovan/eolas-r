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


# ---------------------------------------------------------------------------
# Immigration NZ
# ---------------------------------------------------------------------------

#' Fetch an Immigration NZ dataset
#' @inheritParams eolas_get_statsnz
#' @export
eolas_get_immigration <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Immigration NZ", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Immigration NZ datasets
#' @export
eolas_list_immigration <- function() .eolas_list_source("Immigration NZ")


# ---------------------------------------------------------------------------
# Manaaki Whenua / LRIS
# ---------------------------------------------------------------------------

#' Fetch a Manaaki Whenua / LRIS dataset (land cover, soil, protected areas)
#'
#' A named wrapper over [eolas_get()] for datasets sourced from the Land
#' Resource Information System (LRIS), managed by Manaaki Whenua - Landcare
#' Research NZ. Covers LCDB land-cover vintages (v3.0 through v6), NZLUM land
#' use management, PBC, and the PAN-NZ protected areas layer.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   LCDB v3.0-v4.1 are deprecated vintages retained for longitudinal analysis.
#'   LCDB v5 is superseded by v6 but still served.
#'   `pan_nz_2025_draft` was marked Draft at the time of ingestion (2026-05-12).
#'   Source: \url{https://lris.scinfo.org.nz}.
#'   Licence: CC-BY 4.0 International (LCDB v5/v6, NZLUM, PBC, PAN-NZ);
#'   CC-BY 3.0 NZ (LCDB v3/v4 vintages). Attribution: Manaaki Whenua.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_lris("lcdb_v6_mainland")   # current NZ land cover
#' gdf <- eolas_get_lris("nzlum_v03")          # NZ Land Use Management v0.3
#' gdf <- eolas_get_lris("pan_nz_2025_draft")  # protected areas (Draft, 2025)
#' }
eolas_get_lris <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Manaaki Whenua / LRIS", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Manaaki Whenua / LRIS datasets
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_lris <- function() .eolas_list_source("Manaaki Whenua / LRIS")


# ---------------------------------------------------------------------------
# GeoNet
# ---------------------------------------------------------------------------

#' Fetch a GeoNet dataset (NZ earthquakes, volcanic alert levels, strong-motion sensors)
#'
#' A named wrapper over [eolas_get()] for datasets sourced from GeoNet,
#' operated by Earth Sciences New Zealand (formerly GNS Science). Covers
#' recent NZ earthquake activity (MMI>=3), volcanic alert levels for 12
#' monitored volcanoes, and strong-motion sensor station locations.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   The earthquake catalogue (`geonet_quakes_recent`) is a rolling window of
#'   recent events, not a historical archive. Refreshed every 6 hours.
#'   Source: \url{https://www.geonet.org.nz}.
#'   Licence: CC-BY 3.0 NZ (Earth Sciences New Zealand, formerly GNS Science).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df  <- eolas_get_geonet("geonet_quakes_recent")         # rolling ~100 recent MMI>=3 quakes
#' df  <- eolas_get_geonet("geonet_volcanic_alert_levels") # 12 monitored NZ volcanoes
#' gdf <- eolas_get_geonet("geonet_strong_motion_sensors") # 25 strong-motion stations
#' }
eolas_get_geonet <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "GeoNet", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all GeoNet datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_geonet <- function() .eolas_list_source("GeoNet")


# ---------------------------------------------------------------------------
# DOC (Department of Conservation)
# ---------------------------------------------------------------------------

#' Fetch a DOC (Department of Conservation) dataset
#'
#' A named wrapper over [eolas_get()] for datasets sourced from the
#' Department of Conservation (DOC). Covers public conservation land polygons,
#' hut and campsite locations, walking experiences, tracks, marine reserves,
#' and marine mammal sanctuaries.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Refreshed weekly from DOC's ArcGIS hub. Operational alert streams
#'   (track closures, hazard notices) are wired but currently blocked on
#'   an API key issue; they will appear automatically once resolved.
#'   Source: \url{https://doc.govt.nz}.
#'   Licence: CC-BY 4.0 International (Crown / Department of Conservation).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' huts  <- eolas_get_doc("doc_huts")                        # 1,429 DOC huts (Point)
#' land  <- eolas_get_doc("doc_public_conservation_land")    # ~11k conservation land polygons
#' trks  <- eolas_get_doc("doc_tracks")                      # 3,248 DOC tracks (Polyline)
#' }
eolas_get_doc <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "DOC", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all DOC datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_doc <- function() .eolas_list_source("DOC")
