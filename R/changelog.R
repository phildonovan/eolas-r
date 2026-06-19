# Changelog sync (OUT half of CDC) -- the R port of the Python client's sync_changes.
#
# Incrementally syncs a cdc_serving_tier=changelog dataset via GET /v1/datasets/{name}/changes:
# cold-start baseline (eolas_sync_bulk) + anchor the watermark, then page new changes and pk-merge
# them into the local materialised file (eolas_merge_changes, in cdc.R). v2 sidecar holds the
# watermark; a 410 (watermark expired) self-heals by re-baselining. Keep in lockstep with
# eolas_data/client.py::sync_changes -- both clients must converge on identical current state.

.EOLAS_CHANGES_PAGE_LIMIT <- 50000L
# Max int64 as a STRING -- anchors the cold-start watermark at the feed head without replaying
# history. A numeric literal would serialise in scientific notation and break the server int parse.
.EOLAS_SEQ_MAX <- "4611686018427387904"
.SIDECAR_SCHEMA_VERSION_CDC <- 2L


# Raw GET on /changes with typed, catchable conditions for the known status codes (mirrors
# client.py::_raw_changes_get). Does NOT use eolas_check_status (which would abort on 410 instead
# of letting sync_changes self-heal). Returns the httr2 response on 200.
.eolas_changes_get <- function(name, since_seq, limit, base_url = EOLAS_BASE_URL) {
  key <- eolas_get_key_internal()
  url <- paste0(base_url, "/v1/datasets/", name, "/changes")
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(since_seq = since_seq, limit = limit, format = "parquet") |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- eolas_http_perform(req)
  status <- httr2::resp_status(resp)
  if (status == 200L) return(resp)
  detail <- tryCatch(httr2::resp_body_json(resp)$detail %||% "", error = \(e) "")
  if (status == 402L) {
    cli::cli_abort(c("Incremental sync requires a Pro plan.",
                     i = if (nzchar(detail)) detail else "See https://eolas.fyi/pricing."),
                   class = "eolas_changes_upgrade_required")
  }
  if (status == 403L) {
    if (grepl("licence", tolower(detail))) {
      cli::cli_abort(c("This dataset's licence prohibits export.", i = detail),
                     class = "eolas_changes_licence_restricted")
    }
    cli::cli_abort(if (nzchar(detail)) detail else "API key is inactive.",
                   class = "eolas_auth_error")
  }
  if (status == 410L) {
    body <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
    min_seq <- suppressWarnings(as.numeric(body$min_available_seq %||% 0))
    cli::cli_abort(c("Sync watermark expired -- the requested changes are no longer retained.",
                     i = "Re-baselining from a fresh bulk snapshot."),
                   class = "eolas_watermark_expired",
                   min_available_seq = if (is.na(min_seq)) 0 else min_seq)
  }
  eolas_check_status(resp)  # generic handling for anything else
  resp
}


# Read a Parquet response body (raw bytes) into a data.frame. Via a tempfile for robustness across
# arrow versions; the feed is page-sized so this is cheap.
.eolas_read_parquet_raw <- function(raw) {
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw, tmp)
  as.data.frame(arrow::read_parquet(tmp))
}


# Single tail page -> X-Eolas-Seq-High (the current feed head). Anchors the cold-start watermark.
.eolas_fetch_seq_high <- function(name, since_seq = .EOLAS_SEQ_MAX, base_url = EOLAS_BASE_URL) {
  resp <- tryCatch(.eolas_changes_get(name, since_seq, 1L, base_url = base_url), error = \(e) NULL)
  if (is.null(resp)) return(0)
  hi <- suppressWarnings(as.numeric(httr2::resp_header(resp, "X-Eolas-Seq-High") %||% "0"))
  if (is.na(hi)) 0 else hi
}


# Page through /changes from since_seq -> list(changes = data.frame, final_seq). Stop conditions
# mirror the Python client: explicit X-Eolas-Truncated wins; else row_count < limit heuristic;
# empty page always stops. Propagates the eolas_watermark_expired condition (HTTP 410).
.eolas_fetch_all_change_pages <- function(name, since_seq, base_url = EOLAS_BASE_URL) {
  pages <- list()
  current_seq <- since_seq
  final_seq <- since_seq
  repeat {
    resp <- .eolas_changes_get(name, current_seq, .EOLAS_CHANGES_PAGE_LIMIT, base_url = base_url)
    seq_high <- suppressWarnings(as.numeric(httr2::resp_header(resp, "X-Eolas-Seq-High") %||%
                                            as.character(current_seq)))
    row_count <- suppressWarnings(as.integer(httr2::resp_header(resp, "X-Eolas-Row-Count") %||% "0"))
    trunc_raw <- httr2::resp_header(resp, "X-Eolas-Truncated")
    truncated <- if (!is.null(trunc_raw)) tolower(trunc_raw) == "true"
                 else isTRUE(row_count >= .EOLAS_CHANGES_PAGE_LIMIT)
    if (isTRUE(row_count > 0)) {
      body <- httr2::resp_body_raw(resp)
      if (length(body) > 0) pages[[length(pages) + 1L]] <- .eolas_read_parquet_raw(body)
    }
    final_seq <- seq_high
    if (!isTRUE(truncated) || isTRUE(row_count == 0)) break
    current_seq <- seq_high
  }
  changes <- if (length(pages) == 0L) data.frame() else do.call(rbind, pages)
  list(changes = changes, final_seq = final_seq)
}


# Write the v2 changelog sidecar (byte-compatible shape with the Python client for cross-language
# interop). Lives at paste0(path, ".eolas-meta.json").
.eolas_write_changelog_sidecar <- function(sidecar_path, name, fmt, pk_columns,
                                           current_state_filter, baseline_snapshot_id,
                                           watermark_seq) {
  data <- list(
    schema_version       = .SIDECAR_SCHEMA_VERSION_CDC,
    sync_mode            = "changelog",
    name                 = name,
    format               = fmt,
    pk_columns           = as.list(pk_columns),
    current_state_filter = current_state_filter,
    baseline_snapshot_id = baseline_snapshot_id,
    watermark_seq        = watermark_seq,
    updated_at           = format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
  writeLines(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", pretty = TRUE), sidecar_path)
}


#' Incrementally sync a changelog-tier dataset via the /changes feed
#'
#' The OUT half of CDC. On the first call (cold start) downloads the full baseline via
#' [eolas_sync_bulk()] and anchors the watermark at the current feed head. On subsequent calls pages
#' only the new changes since the watermark and pk-merges them into the local file (atomic rewrite),
#' applying the dataset's `current_state_filter` (e.g. `is_current = true` for SCD2). A `410`
#' (watermark expired) self-heals by re-baselining. Mirrors the Python client's `sync_changes`.
#'
#' @param name Dataset identifier, e.g. `"nz_building_outlines"`.
#' @param path Where to write the materialised Parquet file. The sidecar lives at
#'   `paste0(path, ".eolas-meta.json")`.
#' @param format Only `"parquet"` is supported for changelog sync.
#' @param progress Forwarded to [eolas_sync_bulk()] for the baseline download bar.
#' @param force When `TRUE`, discard the incremental watermark and re-baseline from
#'   a full bulk snapshot.
#' @param base_url API base URL.
#' @return A list with `status`, `sync_mode = "changelog"`, `previous_seq`, `current_seq`,
#'   `ops_applied`, `path`, `current_snapshot_id`.
#' @export
eolas_sync_changes <- function(name, path, format = "parquet", progress = NULL,
                               force = FALSE, base_url = EOLAS_BASE_URL) {
  fmt <- tolower(format)
  if (fmt != "parquet") {
    cli::cli_abort(c("{.fn eolas_sync_changes} only supports {.val parquet}.",
                     x = "Got format = {.val {format}}."))
  }
  out_path <- path.expand(path)
  sidecar_path <- paste0(out_path, ".eolas-meta.json")
  .eolas_apply_force(name, force, base_url = base_url)

  sidecar <- if (file.exists(sidecar_path)) .read_sidecar(sidecar_path) else NULL
  needs_baseline <- is.null(sidecar) ||
    !identical(sidecar$sync_mode, "changelog") ||
    is.null(sidecar$watermark_seq)

  meta <- eolas_info(name, base_url = base_url)
  pk_columns <- meta$pk_columns
  if (is.null(pk_columns) || length(pk_columns) == 0) pk_columns <- sidecar$pk_columns
  # Normalise to a plain character vector -- pk_columns may arrive as a JSON-decoded list (mock) or
  # vector (jsonlite simplifyVector); .eolas_pk_key needs a character vector to index columns.
  pk_columns <- as.character(unlist(pk_columns))
  current_state_filter <- meta$current_state_filter %||% sidecar$current_state_filter

  do_baseline <- function(reason) {
    cli::cli_alert_info("{.field {name}}: {reason} -- baselining from a full bulk snapshot.")
    bulk <- eolas_sync_bulk(name, path = out_path, format = fmt, freshness = "current",
                            progress = progress, force = force, base_url = base_url)
    high <- .eolas_fetch_seq_high(name, base_url = base_url)
    .eolas_write_changelog_sidecar(sidecar_path, name, fmt, pk_columns, current_state_filter,
                                   bulk$current_snapshot_id, high)
    list(status = "downloaded", sync_mode = "changelog", previous_seq = NULL, current_seq = high,
         ops_applied = 0L, path = out_path, current_snapshot_id = bulk$current_snapshot_id)
  }

  if (isTRUE(force)) return(do_baseline("force refresh"))
  if (needs_baseline) return(do_baseline("cold start"))

  prev_watermark <- as.numeric(sidecar$watermark_seq %||% 0)
  baseline_snapshot_id <- sidecar$baseline_snapshot_id %||% ""

  fetched <- tryCatch(
    .eolas_fetch_all_change_pages(name, prev_watermark, base_url = base_url),
    eolas_watermark_expired = function(e) {
      res <- do_baseline("watermark expired")
      res$.expired <- TRUE
      res
    }
  )
  if (isTRUE(fetched$.expired)) {
    fetched$.expired <- NULL
    fetched$previous_seq <- prev_watermark
    fetched$status <- "updated"
    return(fetched)
  }

  changes <- fetched$changes
  if (nrow(changes) == 0) {
    return(list(status = "unchanged", sync_mode = "changelog", previous_seq = prev_watermark,
                current_seq = prev_watermark, ops_applied = 0L, path = out_path,
                current_snapshot_id = baseline_snapshot_id))
  }

  local_df <- if (file.exists(out_path)) as.data.frame(arrow::read_parquet(out_path)) else data.frame()
  merged <- eolas_merge_changes(local_df, changes, pk_columns = pk_columns,
                                current_state_filter = current_state_filter)

  # Atomic write: write to a temp sibling then rename over the target.
  tmp <- paste0(out_path, ".eolas-tmp-", paste(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ""))
  ok <- FALSE
  on.exit(if (!ok && file.exists(tmp)) unlink(tmp), add = TRUE)
  arrow::write_parquet(merged, tmp)
  if (!file.rename(tmp, out_path)) {
    file.copy(tmp, out_path, overwrite = TRUE); unlink(tmp)
  }
  ok <- TRUE

  .eolas_write_changelog_sidecar(sidecar_path, name, fmt, pk_columns, current_state_filter,
                                 baseline_snapshot_id, fetched$final_seq)
  cli::cli_alert_success(
    "Applied {nrow(changes)} change{?s} to {.path {out_path}} (seq {prev_watermark} -> {fetched$final_seq}).")
  list(status = "updated", sync_mode = "changelog", previous_seq = prev_watermark,
       current_seq = fetched$final_seq, ops_applied = nrow(changes), path = out_path,
       current_snapshot_id = baseline_snapshot_id)
}
