# Source-specific get and list functions.
# Each eolas_get_<source>() is a named wrapper over eolas_get() that tags the
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
# EECA (Energy Efficiency and Conservation Authority)
# ---------------------------------------------------------------------------

#' Fetch an EECA dataset (NZ energy use, EV chargers, regional heat demand)
#'
#' A named wrapper over [eolas_get()] for datasets from the Energy Efficiency
#' and Conservation Authority (EECA). Covers NZ energy end-use by sector and
#' fuel type, public and co-funded EV charger locations, quarterly EV
#' penetration metrics by region and territorial authority, and regional
#' industrial process heat demand.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   EV charger streams (`eeca_ev_chargers_public`, `eeca_ev_chargers_cofunded`)
#'   carry point geometry and refresh quarterly.
#'   `eeca_energy_end_use` is the annual Energy End Use Database (EEUD).
#'   `eeca_regional_heat_demand` is an Aug 2024 snapshot.
#'   Source: \url{https://www.eeca.govt.nz/insights/data-tools/}.
#'   Licence: CC-BY 4.0 NZ (Crown).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df  <- eolas_get_eeca("eeca_energy_end_use")         # NZ energy by sector x fuel x end-use x year
#' gdf <- eolas_get_eeca("eeca_ev_chargers_public")     # public EV charging network (Point geometry)
#' df  <- eolas_get_eeca("eeca_ev_metrics_district")    # EV penetration by territorial authority
#' df  <- eolas_get_eeca("eeca_regional_heat_demand")   # industrial process heat by region x sector
#' }
eolas_get_eeca <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "EECA", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all EECA datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_eeca <- function() .eolas_list_source("EECA")


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


# ---------------------------------------------------------------------------
# Auckland Council
# ---------------------------------------------------------------------------

#' Fetch an Auckland Council dataset (overlays, heritage, hazards, zoning)
#'
#' A named wrapper over [eolas_get()] for datasets sourced from the Auckland
#' Council open data hub. Covers district plan overlays, notable trees,
#' significant ecological areas, heritage, and stormwater management zones.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://data-aucklandcouncil.opendata.arcgis.com}.
#'   Licence: CC-BY 4.0 (Auckland Council).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_akl_council("akc_notable_trees_overlay")
#' gdf <- eolas_get_akl_council("akc_significant_ecological_areas_overlay")
#' }
eolas_get_akl_council <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Auckland Council", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Auckland Council datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_akl_council <- function() .eolas_list_source("Auckland Council")


# ---------------------------------------------------------------------------
# Auckland Transport
# ---------------------------------------------------------------------------

#' Fetch an Auckland Transport dataset (roads, public transport, cycling)
#'
#' A named wrapper over [eolas_get()] for datasets sourced from Auckland
#' Transport (AT). Covers bus stops, bus routes, bridges, and cycle
#' infrastructure.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://data-atgis.opendata.arcgis.com}.
#'   Licence: CC-BY 4.0 (Auckland Transport).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_akl_transport("akt_bus_stop")
#' gdf <- eolas_get_akl_transport("akt_cycle_facility_network")
#' }
eolas_get_akl_transport <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Auckland Transport", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Auckland Transport datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_akl_transport <- function() .eolas_list_source("Auckland Transport")


# ---------------------------------------------------------------------------
# Bay of Plenty Councils
# ---------------------------------------------------------------------------

#' Fetch a Bay of Plenty Councils dataset (hazards, resource consents, planning)
#'
#' A named wrapper over [eolas_get()] for datasets from Bay of Plenty Regional
#' Council and its territorial authorities. Covers flood extents, liquefaction,
#' coastal hazards, and planning layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.boprc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_bay_of_plenty("boprc_historic_flood_extents")
#' gdf <- eolas_get_bay_of_plenty("boprc_liquefaction_level_b")
#' }
eolas_get_bay_of_plenty <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Bay of Plenty Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Bay of Plenty Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_bay_of_plenty <- function() .eolas_list_source("Bay of Plenty Councils")


# ---------------------------------------------------------------------------
# Charities Services
# ---------------------------------------------------------------------------

#' Fetch a Charities Services dataset (registered NZ charities)
#'
#' A named wrapper over [eolas_get()] for datasets from Charities Services
#' (a business unit of the Department of Internal Affairs). Covers registered
#' charities, officers, beneficiary groups, and annual financial returns.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame.
#' @details
#'   Source: \url{https://www.charities.govt.nz}.
#'   Licence: Open Government Licence v3.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get_charities("charities_organisations")
#' df <- eolas_get_charities("charities_annual_returns")
#' }
eolas_get_charities <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Charities Services", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Charities Services datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_charities <- function() .eolas_list_source("Charities Services")


# ---------------------------------------------------------------------------
# Co-Lab Waikato
# ---------------------------------------------------------------------------

#' Fetch a Co-Lab Waikato dataset (planning, hazards, heritage across Waikato councils)
#'
#' A named wrapper over [eolas_get()] for datasets aggregated via the Co-Lab
#' Waikato open data hub. Covers district plan zones, coastal hazards,
#' heritage, and building footprints across Waikato-region territorial
#' authorities.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://data-waikatolass.opendata.arcgis.com}.
#'   Licence: CC-BY 4.0 (respective councils).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_colab_waikato("wmkdc_buildings")
#' gdf <- eolas_get_colab_waikato("tcdc_dp_coastal_environment")
#' }
eolas_get_colab_waikato <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Co-Lab Waikato", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Co-Lab Waikato datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_colab_waikato <- function() .eolas_list_source("Co-Lab Waikato")


# ---------------------------------------------------------------------------
# ECan / Canterbury
# ---------------------------------------------------------------------------

#' Fetch an ECan / Canterbury dataset (environment, hazards, resource consents)
#'
#' A named wrapper over [eolas_get()] for datasets from Environment Canterbury
#' (ECan) and Canterbury-region councils. Covers liquefaction, earthquake
#' faults, tsunami zones, water allocation, and resource consents.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://opendata.canterburymaps.govt.nz}.
#'   Licence: CC-BY 4.0 (Environment Canterbury / respective councils).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_ecan_canterbury("ecan_liquefaction_susceptibility_final")
#' gdf <- eolas_get_ecan_canterbury("ecan_tsunami_evacuation_zones")
#' }
eolas_get_ecan_canterbury <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "ECan / Canterbury", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all ECan / Canterbury datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_ecan_canterbury <- function() .eolas_list_source("ECan / Canterbury")


# ---------------------------------------------------------------------------
# Hawke's Bay Councils
# ---------------------------------------------------------------------------

#' Fetch a Hawke's Bay Councils dataset (hazards, planning, coastal management)
#'
#' A named wrapper over [eolas_get()] for datasets from Hawke's Bay Regional
#' Council and its territorial authorities. Covers coastal erosion,
#' liquefaction, flood hazards, and district planning layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.hbrc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_hawkes_bay("hbrc_coastal_erosion_likely_66")
#' gdf <- eolas_get_hawkes_bay("hbrc_chb_hdc_wdc_liquefaction_severity")
#' }
eolas_get_hawkes_bay <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Hawke's Bay Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Hawke's Bay Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_hawkes_bay <- function() .eolas_list_source("Hawke's Bay Councils")


# ---------------------------------------------------------------------------
# Manawatu-Whanganui Councils
# ---------------------------------------------------------------------------

#' Fetch a Manawatu-Whanganui Councils dataset (airsheds, coastal, freshwater)
#'
#' A named wrapper over [eolas_get()] for datasets from Horizons Regional
#' Council (Manawatu-Whanganui) and its territorial authorities. Covers
#' airsheds, coastal marine areas, freshwater, and planning layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.horizons.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_manawatu_whanganui("horizons_coastal_marine_area")
#' gdf <- eolas_get_manawatu_whanganui("horizons_airshed_taihape")
#' }
eolas_get_manawatu_whanganui <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Manawatu-Whanganui Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Manawatu-Whanganui Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_manawatu_whanganui <- function() .eolas_list_source("Manawat\u016b-Whanganui Councils")


# ---------------------------------------------------------------------------
# Napier + Whanganui
# ---------------------------------------------------------------------------

#' Fetch a Napier or Whanganui city dataset (district plan, heritage, infrastructure)
#'
#' A named wrapper over [eolas_get()] for datasets from Napier City Council
#' and Whanganui District Council. Covers district plan precincts, heritage
#' buildings and areas, address points, road centrelines, and parcels.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.napier.govt.nz} / \url{https://www.whanganui.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_napier_whanganui("napier_heritage_buildings")
#' gdf <- eolas_get_napier_whanganui("napier_address_points")
#' }
eolas_get_napier_whanganui <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Napier + Whanganui", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Napier + Whanganui datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_napier_whanganui <- function() .eolas_list_source("Napier + Whanganui")


# ---------------------------------------------------------------------------
# Northland Councils
# ---------------------------------------------------------------------------

#' Fetch a Northland Councils dataset (district plans, designations, heritage)
#'
#' A named wrapper over [eolas_get()] for datasets from Northland Regional
#' Council and its territorial authorities (Far North, Whangarei, Kaipara).
#' Covers district plan zones, designations, heritage, and environmental
#' layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.nrc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_northland("fndc_district_plan_zones")
#' gdf <- eolas_get_northland("fndc_heritage_areas")
#' }
eolas_get_northland <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Northland Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Northland Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_northland <- function() .eolas_list_source("Northland Councils")


# ---------------------------------------------------------------------------
# Otago Councils
# ---------------------------------------------------------------------------

#' Fetch an Otago Councils dataset (land use, water, planning, hazards)
#'
#' A named wrapper over [eolas_get()] for datasets from Otago Regional Council
#' and its territorial authorities (Dunedin, Queenstown-Lakes, Central Otago,
#' Clutha, Waitaki). Covers land use, floodbanks, groundwater protection, and
#' planning layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.orc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_otago("orc_otago_irrigated_areas")
#' gdf <- eolas_get_otago("orc_otago_land_use_2024")
#' }
eolas_get_otago <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Otago Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Otago Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_otago <- function() .eolas_list_source("Otago Councils")


# ---------------------------------------------------------------------------
# PHARMAC
# ---------------------------------------------------------------------------

#' Fetch a PHARMAC dataset (NZ pharmaceutical subsidy schedule + hospital medicines)
#'
#' A named wrapper over [eolas_get()] for datasets from PHARMAC (Pharmaceutical
#' Management Agency). Covers the monthly Pharmaceutical Schedule (community-
#' funded medicines and subsidies) and the Hospital Medicines List (HML),
#' including full longitudinal archives from 2006 and 2011 respectively.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame.
#' @details
#'   Historical archive datasets (`pharmac_schedule_history`,
#'   `pharmac_hml_history`) are append-mode; each month's snapshot is tagged
#'   with a `time_frame` column (YYYY-MM format).
#'   Source: \url{https://schedule.pharmac.govt.nz/}.
#'   Licence: CC-BY 3.0 NZ (Crown).
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' df <- eolas_get_pharmac("pharmac_schedule")          # current month's funded medicines
#' df <- eolas_get_pharmac("pharmac_schedule_history")  # 2006-present subsidy archive
#' df <- eolas_get_pharmac("pharmac_hospital_medicines_list")  # current HML
#' df <- eolas_get_pharmac("pharmac_hml_history")       # 2011-present HML archive
#' }
eolas_get_pharmac <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "PHARMAC", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all PHARMAC datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_pharmac <- function() .eolas_list_source("PHARMAC")


# ---------------------------------------------------------------------------
# Southland Councils
# ---------------------------------------------------------------------------

#' Fetch a Southland Councils dataset (district plans, coastal, natural hazards)
#'
#' A named wrapper over [eolas_get()] for datasets from Environment Southland
#' and its territorial authorities (Southland District, Gore, Invercargill).
#' Covers district plan zones, coastal hazards, heritage, and land use.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.es.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_southland("sdc_southland_dp_zones")
#' gdf <- eolas_get_southland("sdc_southland_dp_heritage_items")
#' }
eolas_get_southland <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Southland Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Southland Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_southland <- function() .eolas_list_source("Southland Councils")


# ---------------------------------------------------------------------------
# Taranaki Councils
# ---------------------------------------------------------------------------

#' Fetch a Taranaki Councils dataset (coastal, biodiversity, district plans)
#'
#' A named wrapper over [eolas_get()] for datasets from Taranaki Regional
#' Council and its territorial authorities (New Plymouth, Stratford, South
#' Taranaki). Covers biodiversity, coastal management, and district planning
#' layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.trc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_taranaki("trc_biodiversity_coastal_mgmt_areas")
#' gdf <- eolas_get_taranaki("npdc_dp_operative_coastal_flooding")
#' }
eolas_get_taranaki <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Taranaki Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Taranaki Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_taranaki <- function() .eolas_list_source("Taranaki Councils")


# ---------------------------------------------------------------------------
# Gisborne / Top of South Councils
# ---------------------------------------------------------------------------

#' Fetch a Gisborne / Top of South Councils dataset (coastal, planning, heritage)
#'
#' A named wrapper over [eolas_get()] for datasets from Gisborne District
#' Council, Marlborough District Council, Nelson City Council, and Tasman
#' District Council. Covers coastal hazards, planning zones, and heritage
#' layers.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.gdc.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_top_of_south("gdc_coastal_environment")
#' gdf <- eolas_get_top_of_south("gdc_coastal_erosion")
#' }
eolas_get_top_of_south <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Gisborne / Top of South Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Gisborne / Top of South Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_top_of_south <- function() .eolas_list_source("Gisborne / Top of South Councils")


# ---------------------------------------------------------------------------
# Wellington Region Councils
# ---------------------------------------------------------------------------

#' Fetch a Wellington Region Councils dataset (hazards, planning, infrastructure)
#'
#' A named wrapper over [eolas_get()] for datasets from Greater Wellington
#' Regional Council and its territorial authorities (Wellington, Hutt, Upper
#' Hutt, Porirua, Kapiti Coast). Covers flood and earthquake hazards, district
#' plan zones, and coastal inundation.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.gw.govt.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_wellington("wcc_district_plan_zones_2024")
#' gdf <- eolas_get_wellington("gwrc_flood_hazard_extents")
#' }
eolas_get_wellington <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "Wellington Region Councils", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all Wellington Region Councils datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_wellington <- function() .eolas_list_source("Wellington Region Councils")


# ---------------------------------------------------------------------------
# West Coast (Te Tai o Poutini)
# ---------------------------------------------------------------------------

#' Fetch a West Coast (Te Tai o Poutini) dataset (faults, landslides, planning)
#'
#' A named wrapper over [eolas_get()] for datasets from West Coast Regional
#' Council (Te Tai o Poutini) and its territorial authorities (Buller, Grey,
#' Westland). Covers active faults, the Alpine Fault, landslide catalogs, and
#' significant natural areas.
#'
#' @inheritParams eolas_get_statsnz
#' @return A `eolas_dataset` data frame, or an `sf` object when geometry is
#'   present and conversion is enabled.
#' @details
#'   Source: \url{https://www.ttpp.nz}.
#'   Licence: CC-BY 4.0.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key")
#' gdf <- eolas_get_west_coast("wcrc_active_faults")
#' gdf <- eolas_get_west_coast("wcrc_alpine_fault_traces")
#' }
eolas_get_west_coast <- function(name, start = NULL, end = NULL, limit = NULL, as_sf = NULL) {
  .eolas_get_source(name, "West Coast (Te Tai o Poutini)", start = start, end = end, limit = limit, as_sf = as_sf)
}

#' List all West Coast (Te Tai o Poutini) datasets available in eolas
#' @return A data frame (tibble if available) of dataset metadata.
#' @export
eolas_list_west_coast <- function() .eolas_list_source("West Coast (Te Tai o Poutini)")
