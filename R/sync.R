# Unified sync dispatcher -- routes on the dataset's CDC serving tier (mirrors the Python client's
# Client.sync()). changelog-tier datasets get incremental /changes sync; everything else gets the
# full-snapshot bulk path. The caller does not need to know which tier a dataset is.

#' Sync a dataset to a local file, auto-routing on its CDC serving tier
#'
#' Reads `cdc_serving_tier` from the dataset metadata and dispatches:
#' * `"changelog"` -> [eolas_sync_changes()] -- incremental /changes feed, pk-merged into the local file
#'   (first call downloads a baseline; later calls apply only what changed).
#' * anything else (`"snapshot"`) -> [eolas_sync_bulk()] -- full-snapshot download, refreshed when the
#'   server snapshot changes.
#'
#' Both paths keep a `paste0(path, ".eolas-meta.json")` sidecar and return a list with at least
#' `status`, `path`, and `current_snapshot_id`; the changelog path additionally returns `sync_mode`,
#' `previous_seq`, `current_seq`, and `ops_applied`.
#'
#' @param name Dataset identifier, e.g. `"nz_building_outlines"`.
#' @param path Local file to materialise.
#' @param format Output format. Changelog sync requires `"parquet"`; bulk also accepts `"csv_gz"` /
#'   `"geoparquet"`.
#' @param progress Tri-state progress bar control forwarded to the underlying sync.
#' @param force When `TRUE`, bypass local "unchanged" cache and re-sync from the
#'   server. On changelog-tier datasets this re-baselines from a full bulk snapshot.
#' @param base_url API base URL.
#' @return The result list from the dispatched sync (see [eolas_sync_changes()] / [eolas_sync_bulk()]).
#' @export
#' @examples
#' \dontrun{
#' # Same call works whether the dataset is snapshot- or changelog-tier:
#' eolas_sync("nz_building_outlines", path = "buildings.parquet")
#' }
eolas_sync <- function(name, path, format = "parquet", progress = NULL,
                       force = FALSE, base_url = EOLAS_BASE_URL) {
  meta <- eolas_info(name, base_url = base_url)
  tier <- meta$cdc_serving_tier %||% "snapshot"
  if (identical(tier, "changelog")) {
    eolas_sync_changes(name, path = path, format = format, progress = progress,
                       force = force, base_url = base_url)
  } else {
    eolas_sync_bulk(name, path = path, format = format, progress = progress,
                    force = force, base_url = base_url)
  }
}
