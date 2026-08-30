# Dataset metadata -- session cache, attr attachment, accessors.
#
# Table- and column-level metadata come from GET /v1/datasets/{name}. We fetch
# once per (base_url, name) per R session and attach quietly to returned
# datasets -- never as data columns.

.eolas_meta_cache <- new.env(parent = emptyenv())

# Normalise one JSON field to a length-1 value for a one-row tibble. NULL and
# length-0 vectors become NA; multi-element vectors/lists become list-columns.
.eolas_info_scalar <- function(v) {
  if (is.null(v) || length(v) == 0L) return(NA)
  if (length(v) == 1L) return(v)
  list(v)
}

.eolas_parse_info_columns <- function(columns_raw) {
  if (is.null(columns_raw) || length(columns_raw) == 0L) return(NULL)
  rows <- lapply(columns_raw, function(col) {
    data.frame(
      name        = col$name %||% NA_character_,
      type        = col$type %||% NA_character_,
      description = col$description %||% NA_character_,
      series_id   = if (is.null(col$series_id) || length(col$series_id) == 0L) {
        NA_character_
      } else {
        col$series_id
      },
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

#' @keywords internal
.eolas_parse_info_response <- function(body) {
  columns_raw <- body$columns
  body$columns <- NULL
  info <- tibble::as_tibble(lapply(body, .eolas_info_scalar))
  col_df <- .eolas_parse_info_columns(columns_raw)
  if (!is.null(col_df)) info$columns <- list(col_df)
  info
}

.eolas_meta_cache_key <- function(name, base_url) {
  paste0(base_url, "::", name)
}

#' @keywords internal
.eolas_info_cached <- function(name, base_url = EOLAS_BASE_URL) {
  key <- .eolas_meta_cache_key(name, base_url)
  if (exists(key, envir = .eolas_meta_cache, inherits = FALSE)) {
    return(get(key, envir = .eolas_meta_cache, inherits = FALSE))
  }
  info <- eolas_info(name, base_url = base_url)
  assign(key, info, envir = .eolas_meta_cache)
  info
}

# Drop session-cached eolas_info() for one dataset (or all when name is NULL).
.eolas_meta_cache_clear <- function(name = NULL, base_url = EOLAS_BASE_URL) {
  if (is.null(name)) {
    keys <- ls(.eolas_meta_cache, all.names = TRUE)
    if (length(keys)) rm(list = keys, envir = .eolas_meta_cache)
    return(invisible(length(keys)))
  }
  key <- .eolas_meta_cache_key(name, base_url)
  if (exists(key, envir = .eolas_meta_cache, inherits = FALSE)) {
    rm(list = key, envir = .eolas_meta_cache)
    invisible(1L)
  } else {
    invisible(0L)
  }
}

# force=TRUE: forget cached metadata before the next fetch/routing decision.
.eolas_apply_force <- function(name, force, base_url = EOLAS_BASE_URL) {
  if (isTRUE(force)) .eolas_meta_cache_clear(name, base_url = base_url)
  invisible(NULL)
}

.eolas_table_meta <- function(info) {
  if (is.null(info) || !is.data.frame(info) || nrow(info) < 1L) return(NULL)
  drop <- intersect("columns", names(info))
  if (length(drop)) info <- info[, setdiff(names(info), drop), drop = FALSE]
  if (ncol(info) < 1L) return(NULL)
  tibble::as_tibble(info[1, , drop = FALSE])
}

.eolas_column_meta <- function(info) {
  if (is.null(info) || !is.data.frame(info) || nrow(info) < 1L) return(NULL)
  if (!"columns" %in% names(info)) return(NULL)
  cols <- info$columns[[1]]
  if (is.null(cols) || (is.list(cols) && length(cols) == 0L)) return(NULL)
  if (is.data.frame(cols)) return(tibble::as_tibble(cols))
  if (is.list(cols) && length(cols) > 0L && is.list(cols[[1]])) {
    return(tibble::as_tibble(
      do.call(rbind, lapply(cols, as.data.frame, stringsAsFactors = FALSE))
    ))
  }
  NULL
}

# Mirror of the API live-data guard (`live_pull_guard` in
# api/app/routes/datasets.py, architecture.md 2.5.4c, 2026-08-30) and the
# Python client. Unless a *real* date filter (start/end on a table that has a
# date column) narrows the scan, a live pull of a dataset above this row count
# OR carrying geometry (unless geometry = FALSE projects it away) is refused
# with HTTP 413 -- EXCEPT for a small slice: 0 < limit <= 10,000 (the R client
# has no `dimensions` argument; on the API a `dimensions` filter defeats the
# slice). limit = 0, limit > 100,000 and anything between 10,000 and 100,000
# are all refused alike; a positive limit is not a back door.
.EOLAS_LARGE_DATASET_ROW_THRESHOLD <- 100000L
.EOLAS_SAFE_LIVE_SLICE_ROWS <- 10000L

# Exactly the server's safe-slice test: 0 < limit <= 10,000. NULL and 0 both
# mean "whole dataset" and are not a slice.
.eolas_live_slice_allowed <- function(limit) {
  if (is.null(limit)) return(FALSE)
  limit <- suppressWarnings(as.integer(limit))
  isTRUE(limit > 0L && limit <= .EOLAS_SAFE_LIVE_SLICE_ROWS)
}

.eolas_meta_truthy <- function(meta, field) {
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) return(FALSE)
  if (!field %in% names(meta)) return(FALSE)
  isTRUE(meta[[field]][[1]])
}

.eolas_bulk_export_allowed <- function(meta) {
  cls <- tolower(.eolas_dataset_field(meta, "bulk_export_class", "") %||% "")
  nzchar(cls) && cls != "none"
}

# TRUE when a live /data pull of `meta` with no date bounds would be a 413:
# the dataset is large or spatial AND the request is not a safe slice
# (`limit`; NULL = unbounded, which is what bulk routing asks about).
#
# geometry = FALSE asks the API to project geometry_wkt away at the scan, which
# is exactly the trigger the server drops from its own 413 guard -- so skip the
# geometry checks here too. Leaving them in would keep routing attributes-only
# pulls to a bulk download, defeating the point. The row-count trigger below
# still applies: dropping a column does not reduce the number of rows.
#
# Callers applying a real start/end date filter must not consult this at all:
# a binding date filter is pushed into the Iceberg scan and skips the guard
# server-side. (start/end on a date-less table does NOT count -- the server
# 400s that; this client strips them first, with a warning.)
.eolas_live_pull_blocked <- function(meta, geometry = TRUE, limit = NULL) {
  if (.eolas_live_slice_allowed(limit)) return(FALSE)
  if (!isFALSE(geometry)) {
    if (.eolas_meta_truthy(meta, "has_geometry")) return(TRUE)
    gt <- .eolas_dataset_field(meta, "geometry_type", "")
    wkt <- .eolas_dataset_field(meta, "geometry_wkt", "")
    if (nzchar(gt) && !identical(tolower(gt), "none")) return(TRUE)
    if (nzchar(wkt) && !identical(tolower(wkt), "none")) return(TRUE)
  }
  row_count <- suppressWarnings(as.integer(
    .eolas_dataset_field(meta, "row_count_at_last_refresh", 0L)
  ))
  isTRUE(row_count > .EOLAS_LARGE_DATASET_ROW_THRESHOLD)
}

.eolas_resolve_fetch_limit <- function(limit) {
  if (is.null(limit)) return(list(fetch = 0L, user = NULL))
  limit <- as.integer(limit)
  if (limit <= 0L) return(list(fetch = limit, user = limit))
  list(fetch = 0L, user = limit)
}

.eolas_apply_row_limit <- function(df, user_limit) {
  if (is.null(user_limit) || user_limit <= 0L || !nrow(df)) return(df)
  if ("date" %in% names(df)) {
    n <- nrow(df)
    if (n <= user_limit) return(df)
    out <- df[seq.int(n - user_limit + 1L, n), , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  out <- df[seq_len(min(user_limit, nrow(df))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.eolas_warn_source_mismatch <- function(name, expected_source, meta) {
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) return(invisible())
  if (!"source" %in% names(meta)) return(invisible())
  actual <- meta$source[[1]] %||% ""
  if (!nzchar(actual) || identical(actual, expected_source)) return(invisible())
  msgs <- c(
    "{.field {name}} is sourced from {.val {actual}}, not {.val {expected_source}}.",
    "i" = "See {.fn eolas_info} for canonical metadata."
  )
  if (identical(name, "nz_cpi") && identical(expected_source, "Stats NZ")) {
    msgs <- c(
      msgs,
      "i" = "{.code nz_cpi} is OECD annual % change. For CPI index levels use {.code rbnz_m1_prices} ({.val RBNZ})."
    )
  }
  cli::cli_warn(msgs)
  invisible()
}

.eolas_fetch_meta_info <- function(name, base_url, meta) {
  if (!isTRUE(meta)) return(NULL)
  tryCatch(.eolas_info_cached(name, base_url = base_url), error = function(e) NULL)
}

# Columns recognised by the API for /data?start=&end= filtering (streaming.R).
.EOLAS_DATE_FILTER_CANDIDATES <- c(
  "date", "time_frame", "open_date", "awarded_date", "start_date"
)

#' @keywords internal
.eolas_date_filter_column <- function(meta_info) {
  if (is.null(meta_info) || !is.data.frame(meta_info) || nrow(meta_info) < 1L) {
    return(NULL)
  }
  explicit <- .eolas_dataset_field(meta_info, "date_filter_column", fallback = NA_character_)
  if (!is.na(explicit) && nzchar(as.character(explicit))) return(as.character(explicit))
  if (!"columns" %in% names(meta_info)) return(NULL)
  col_df <- meta_info$columns[[1]]
  if (is.null(col_df) || !is.data.frame(col_df) || nrow(col_df) < 1L) return(NULL)
  hit <- .EOLAS_DATE_FILTER_CANDIDATES[.EOLAS_DATE_FILTER_CANDIDATES %in% col_df$name]
  if (length(hit)) hit[[1]] else NULL
}

#' @keywords internal
.eolas_resolve_date_bounds <- function(meta_info, start, end) {
  if (is.null(start) && is.null(end)) {
    return(list(start = NULL, end = NULL, stripped = FALSE))
  }
  if (is.null(meta_info)) {
    return(list(start = start, end = end, stripped = FALSE))
  }
  if (!is.null(.eolas_date_filter_column(meta_info))) {
    return(list(start = start, end = end, stripped = FALSE))
  }
  list(start = NULL, end = NULL, stripped = TRUE)
}

# Safe scalar read from a one-row eolas_info tibble.  Using `$` on a tibble
# warns when the column is absent (e.g. API has `name` but not `table`).
.eolas_dataset_field <- function(meta, field, fallback = NULL) {
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) return(fallback)
  if (!field %in% names(meta)) return(fallback)
  val <- meta[[field]][[1]]
  if (length(val) == 0L || (length(val) == 1L && is.na(val))) return(fallback)
  val
}

.eolas_provenance_from_headers <- function(resp) {
  if (is.null(resp)) return(list())
  get_hdr <- function(key) {
    val <- httr2::resp_header(resp, key)
    if (is.null(val) || !nzchar(val)) return(NULL)
    val
  }
  out <- list()
  if (!is.null(v <- get_hdr("X-Eolas-Attribution"))) out$attribution_text <- v
  if (!is.null(v <- get_hdr("X-Eolas-Licence"))) out$licence <- v
  if (!is.null(v <- get_hdr("X-Eolas-Source"))) out$source <- v
  if (!is.null(v <- get_hdr("X-Eolas-Source-URL"))) out$source_url <- v
  if (!is.null(v <- get_hdr("X-Eolas-Namespace"))) out$namespace <- v
  out
}

# Plan-cap truncation contract (C22). The server sets X-Eolas-Truncated: true
# (plus X-Plan-Row-Cap / X-Plan) when the caller's plan capped the row window
# below what was requested. Returns list() when the header is absent (old
# server / bulk path), otherwise list(truncated =, row_cap =, plan =).
.eolas_truncation_from_headers <- function(resp) {
  if (is.null(resp)) return(list())
  raw <- tryCatch(httr2::resp_header(resp, "X-Eolas-Truncated"), error = function(e) NULL)
  if (is.null(raw)) return(list())
  out <- list(truncated = identical(tolower(trimws(raw)), "true"))
  cap <- tryCatch(httr2::resp_header(resp, "X-Plan-Row-Cap"), error = function(e) NULL)
  if (!is.null(cap)) {
    cap_int <- suppressWarnings(as.integer(cap))
    out$row_cap <- if (is.na(cap_int)) NA_integer_ else cap_int
  }
  plan <- tryCatch(httr2::resp_header(resp, "X-Plan"), error = function(e) NULL)
  if (!is.null(plan) && nzchar(plan)) out$plan <- plan
  out
}

# Stamp truncated / row_cap onto the one-row metadata tibble so a capped slice
# is never presented as the full dataset. Creates the tibble when meta = FALSE.
.eolas_merge_truncation <- function(meta_info, trunc) {
  if (length(trunc) == 0L) return(meta_info)
  if (is.null(meta_info) || !is.data.frame(meta_info) || nrow(meta_info) < 1L) {
    meta_info <- tibble::tibble(.rows = 1L)
  }
  meta_info$truncated <- isTRUE(trunc$truncated)
  if (isTRUE(trunc$truncated)) {
    meta_info$row_cap <- if (is.null(trunc$row_cap)) NA_integer_ else trunc$row_cap
  }
  meta_info
}

# Warn (cli) when a /data response was capped by the plan. `user_limit` is the
# caller's limit= so the message can say the "latest N" came from inside the
# capped slice, not from the dataset.
.eolas_warn_if_truncated <- function(name, resp, user_limit = NULL) {
  trunc <- .eolas_truncation_from_headers(resp)
  if (!isTRUE(trunc$truncated)) return(invisible(trunc))
  cap <- trunc$row_cap
  cap_txt <- if (!is.null(cap) && !is.na(cap)) {
    paste0(format(cap, big.mark = ","), " rows")
  } else {
    "the plan row cap"
  }
  plan_txt <- if (!is.null(trunc$plan)) paste0(" (", trunc$plan, " plan)") else ""
  msgs <- c(
    "!" = paste0(
      "{.val {name}}: response truncated to ", cap_txt, plan_txt,
      ". This is a file-order slice, NOT the full dataset."
    )
  )
  if (!is.null(user_limit) && user_limit > 0L) {
    msgs <- c(msgs, "!" = paste0(
      "{.code limit = ", user_limit, "} returned the latest rows WITHIN that slice, ",
      "not the dataset's most recent rows."
    ))
  }
  msgs <- c(msgs, "i" = paste0(
    "Check {.code eolas_meta(df)$truncated}. Use {.code start=}/{.code end=} to narrow, ",
    "{.fn eolas_get_local} / {.fn eolas_sync_bulk} for the whole table, ",
    "or upgrade at {.url https://eolas.fyi/pricing}."
  ))
  cli::cli_warn(msgs)
  invisible(trunc)
}

.eolas_merge_provenance <- function(meta_info, provenance) {
  if (length(provenance) == 0L) return(meta_info)
  if (is.null(meta_info) || !is.data.frame(meta_info) || nrow(meta_info) < 1L) {
    return(tibble::as_tibble(as.list(provenance)))
  }
  for (nm in names(provenance)) {
    if (!is.null(provenance[[nm]]) && nzchar(as.character(provenance[[nm]]))) {
      meta_info[[nm]] <- list(provenance[[nm]])
    }
  }
  meta_info
}

.eolas_attach_dataset_meta <- function(x, name, source = NULL, meta_info = NULL) {
  if (!is.null(source) && nzchar(source)) attr(x, "eolas_source") <- source
  attr(x, "eolas_name")   <- name
  attr(x, "eolas_meta")    <- .eolas_table_meta(meta_info)
  attr(x, "eolas_columns") <- .eolas_column_meta(meta_info)
  x
}

# Promote an sf object's attribute table to tbl_df without touching geometry.
.eolas_sf_as_tibble <- function(x) {
  if (!inherits(x, "sf") || inherits(x, "tbl_df")) return(x)
  attrs <- tibble::as_tibble(sf::st_drop_geometry(x))
  sf::st_sf(attrs, geometry = sf::st_geometry(x))
}

# Whole-dataset pulls on large/geo tables -> eolas_get_local() (CDN bulk cache).
# Returns the local result, or NULL to fall through to the live API path.
.eolas_maybe_route_get_local <- function(name, as_sf = NULL, meta = TRUE,
                                         progress = NULL, force = FALSE,
                                         base_url = EOLAS_BASE_URL,
                                         geometry = TRUE, ...) {
  meta_info <- tryCatch(
    if (isTRUE(meta)) .eolas_info_cached(name, base_url = base_url) else NULL,
    error = function(e) NULL
  )
  if (is.null(meta_info) ||
      !.eolas_bulk_export_allowed(meta_info) ||
      !.eolas_live_pull_blocked(meta_info, geometry = geometry)) {
    return(NULL)
  }
  # Routing decision is final -- never fall back to the live /data path (413).
  # The bulk artifact always carries geometry_wkt -- there is no server-side
  # projection on that path. If the caller asked for geometry = FALSE we must
  # honour it here, or a >100k-row spatial table (still blocked by the row-count
  # trigger, so still routed) silently returns the full geometry-bearing file,
  # and with as_sf = NULL even auto-converts to sf -- the exact opposite of what
  # was requested. Found in review, 2026-07-22.
  # geometry is passed DOWN rather than applied after: eolas_get_local() projects
  # the column away at read time, so the WKB/WKT is never decoded and the entire
  # sf conversion path is skipped. The cached file itself is untouched, so one
  # artifact still serves both variants.
  eolas_get_local(
    name = name, as_sf = as_sf, as_arrow = FALSE, meta = meta,
    progress = progress, force = force, geometry = geometry,
    base_url = base_url, ...
  )
}

.eolas_finalize_dataset <- function(x, name, meta_info = NULL, source = NULL) {
  if (inherits(x, "arrow_tabular")) return(x)
  if (inherits(x, "sf")) {
    x <- .eolas_sf_as_tibble(x)
    x <- .eolas_attach_dataset_meta(x, name = name, source = source, meta_info = meta_info)
    return(structure(x, class = c("eolas_dataset", class(x))))
  }
  new_eolas_dataset(x, name = name, source = source, meta_info = meta_info)
}

.eolas_print_meta_subtitle <- function(x) {
  meta <- attr(x, "eolas_meta")
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) return(invisible())
  parts <- character(0)
  title <- if ("title" %in% names(meta)) meta$title[[1]] %||% "" else ""
  if (nzchar(title)) parts <- c(parts, title)
  cadence <- if ("refresh_cadence" %in% names(meta)) meta$refresh_cadence[[1]] %||% "" else ""
  if (nzchar(cadence)) parts <- c(parts, paste0("refreshed ", cadence))
  if ("truncated" %in% names(meta) && isTRUE(meta$truncated[[1]])) {
    cap <- if ("row_cap" %in% names(meta)) meta$row_cap[[1]] else NA
    parts <- c(parts, if (!is.null(cap) && !is.na(cap)) {
      paste0("TRUNCATED to ", format(cap, big.mark = ","), " rows by plan cap")
    } else {
      "TRUNCATED by plan cap"
    })
  }
  if (length(parts)) cli::cli_text(paste(parts, collapse = " \u00b7 "))
  invisible()
}


#' Dataset metadata attached by eolas fetch functions
#'
#' Returns the one-row metadata tibble attached by [eolas_get()],
#' [eolas_get_local()], and source-specific getters -- title, description,
#' licence, refresh cadence, and provenance fields. The full `description`
#' is available here but is **not** printed by default; call this accessor
#' when you need the prose.
#'
#' @param x An object returned by an `eolas_get*()` function (`eolas_dataset`,
#'   or `sf` when geospatial conversion is enabled).
#' @return A one-row tibble, or `NULL` when metadata was not attached
#'   (e.g. `meta = FALSE` on the fetch call).
#' @export
#' @examples
#' \dontrun{
#' df <- eolas_get("nz_cpi", limit = 10)
#' eolas_meta(df)$description
#' }
eolas_meta <- function(x) {
  if (!inherits(x, c("eolas_dataset", "sf"))) {
    cli::cli_abort("{.arg x} must be an {.cls eolas_dataset} or {.pkg sf} object from eolas.")
  }
  attr(x, "eolas_meta")
}


#' Column description for a dataset returned by eolas
#'
#' Looks up the human-readable gloss for a column name from the metadata
#' attached at fetch time (built server-side from the Iceberg schema).
#'
#' @param x An `eolas_dataset` or `sf` object from eolas.
#' @param column Column name, e.g. `"value"`.
#' @return Character description, or `NULL` if unknown / not attached.
#' @export
#' @examples
#' \dontrun{
#' df <- eolas_get("nz_cpi", limit = 10)
#' eolas_column_label(df, "value")
#' }
eolas_column_label <- function(x, column) {
  if (!inherits(x, c("eolas_dataset", "sf"))) {
    cli::cli_abort("{.arg x} must be an {.cls eolas_dataset} or {.pkg sf} object from eolas.")
  }
  cols <- attr(x, "eolas_columns")
  if (is.null(cols) || !is.data.frame(cols) || !"name" %in% names(cols)) return(NULL)
  idx <- match(column, cols$name)
  if (is.na(idx)) return(NULL)
  desc <- cols$description[[idx]]
  if (is.null(desc) || !nzchar(desc)) return(NULL)
  desc
}