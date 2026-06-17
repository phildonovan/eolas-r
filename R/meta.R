# Dataset metadata — session cache, attr attachment, accessors.
#
# Table- and column-level metadata come from GET /v1/datasets/{name}. We fetch
# once per (base_url, name) per R session and attach quietly to returned
# datasets — never as data columns.

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

.eolas_finalize_dataset <- function(x, name, meta_info = NULL, source = NULL) {
  if (inherits(x, "arrow_tabular")) return(x)
  if (inherits(x, "sf")) {
    return(.eolas_attach_dataset_meta(x, name = name, source = source, meta_info = meta_info))
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
  if (length(parts)) cli::cli_text(paste(parts, collapse = " \u00b7 "))
  invisible()
}


#' Dataset metadata attached by eolas fetch functions
#'
#' Returns the one-row metadata tibble attached by [eolas_get()],
#' [eolas_get_local()], and source-specific getters — title, description,
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