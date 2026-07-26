# Client-side long/wide reshape helpers (2026-07-27).
#
# eolas_pivot_longer() / eolas_pivot_wider() reshape a data frame returned by
# eolas_get() / eolas_get_local() using the `layout` / `time_columns` /
# `id_columns` / `value_columns` / `measure_name_column` metadata attached to
# it as the `eolas_meta` attribute (see meta.R / .eolas_attach_dataset_meta).
#
# These are deliberately EXPLICIT, separate functions -- they are never
# called automatically by eolas_get()/eolas_get_local(), and they REFUSE
# rather than guess a shape:
#
#   * no `layout` metadata attached (and none derivable via `name =`)
#     -> error.
#   * layout %in% c("feature", "entity") -> refuse (a feature/entity row is
#     the natural grain; melting or pivoting is meaningless).
#   * a geometry column (`geometry_wkt`, or an `sf` object) is present
#     -> refuse (melting geometry repeats the WKT/WKB payload once per
#     measure row).
#
# This mirrors the reasoning that led to eolas_plot() being removed in
# v1.3.0 -- see dataset.R:30-36: eolas does not ship a helper that has to
# *guess* per-dataset shape from column names like date/period/value.

# Extract a metadata field from the one-row `eolas_meta` tibble as a plain
# character vector. Multi-element JSON arrays are stored as list(v) by
# .eolas_info_scalar(); a length-1 field is stored directly. Handle both.
.eolas_meta_vec <- function(meta, field) {
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) {
    return(NULL)
  }
  if (!field %in% names(meta)) {
    return(NULL)
  }
  val <- meta[[field]][[1]]
  if (is.null(val) || (length(val) == 1L && is.na(val))) {
    return(NULL)
  }
  if (is.list(val)) val <- unlist(val, use.names = FALSE)
  out <- as.character(val)
  if (!length(out)) {
    return(NULL)
  }
  out
}

.eolas_first <- function(v) {
  if (is.null(v) || !length(v)) {
    return(NULL)
  }
  v[[1]]
}

# Resolve the eolas_meta attached to `x`, or fetch it fresh by `name`. Never
# infers a shape when metadata is entirely unavailable -- errors instead.
.eolas_reshape_meta <- function(x, name = NULL, base_url = EOLAS_BASE_URL) {
  meta <- attr(x, "eolas_meta")
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) {
    nm <- name %||% attr(x, "eolas_name")
    if (!is.null(nm)) {
      meta <- tryCatch(.eolas_info_cached(nm, base_url = base_url), error = function(e) NULL)
    }
  }
  if (is.null(meta) || !is.data.frame(meta) || nrow(meta) < 1L) {
    cli::cli_abort(c(
      "Cannot reshape: no eolas layout metadata attached to {.arg x}.",
      "i" = "Pass a dataset from {.fn eolas_get} / {.fn eolas_get_local} with {.code meta = TRUE} (the default), or supply {.arg name} to fetch metadata directly."
    ))
  }
  meta
}

# Shared refusal gate for eolas_pivot_longer()/eolas_pivot_wider(). Returns
# the lower-cased layout string on success.
.eolas_reshape_guard <- function(x, meta, name = NULL) {
  ds_name <- name %||% attr(x, "eolas_name") %||% "this dataset"
  layout <- .eolas_first(.eolas_meta_vec(meta, "layout"))
  if (is.null(layout) || !nzchar(layout)) {
    cli::cli_abort(c(
      "Cannot reshape {.val {ds_name}}: no {.field layout} metadata.",
      "i" = "eolas refuses to guess a date/period/value shape from column names -- this is exactly the silent-wrongness class {.fn eolas_plot} was removed for in v1.3.0.",
      "i" = "See {.url https://docs.eolas.fyi} for the layout contract."
    ))
  }
  layout <- tolower(layout)
  if (layout %in% c("feature", "entity")) {
    kind <- if (identical(layout, "feature")) {
      "a feature table (one row per real-world entity, e.g. a parcel or address)"
    } else {
      "an entity table (one row per case/claim/award)"
    }
    cli::cli_abort(
      "Cannot reshape {.val {ds_name}}: layout is {.val {layout}} -- {kind}. Melting or pivoting would be meaningless."
    )
  }
  if ("geometry_wkt" %in% names(x) || inherits(x, "sf")) {
    cli::cli_abort(c(
      "Cannot reshape {.val {ds_name}}: a geometry column is present.",
      "i" = "Melting geometry repeats the WKT/WKB payload once per measure row -- drop it first with {.code geometry = FALSE} on the fetch call."
    ))
  }
  layout
}

# Rewrite the eolas layout metadata on a reshaped frame so it describes the NEW
# shape, not the source shape. tidyr::pivot_longer/pivot_wider drop custom
# attributes, so without this the result carries no layout and the reverse
# pivot fails with "no metadata". Stored as the same one-row tibble shape
# .eolas_meta_vec() reads (multi-element fields as list-columns).
.eolas_attach_reshaped_meta <- function(out, source, layout, time_cols, id_cols,
                                        value_cols, measure_col) {
  keep_time <- intersect(as.character(time_cols), names(out))
  attr(out, "eolas_meta") <- tibble::tibble(
    layout = layout,
    time_columns = list(keep_time),
    id_columns = list(intersect(as.character(id_cols), names(out))),
    value_columns = list(as.character(value_cols)),
    measure_name_column = if (is.null(measure_col) || !nzchar(measure_col)) "" else measure_col
  )
  nm <- attr(source, "eolas_name")
  if (!is.null(nm)) attr(out, "eolas_name") <- nm
  out
}

# Column-name -> series_id from the eolas_columns glossary attribute, if any.
.eolas_series_id_map <- function(x) {
  cols <- attr(x, "eolas_columns")
  if (is.null(cols) || !is.data.frame(cols) || !all(c("name", "series_id") %in% names(cols))) {
    return(NULL)
  }
  sid <- as.character(cols$series_id)
  nm <- as.character(cols$name)
  keep <- !is.na(sid) & nzchar(sid)
  if (!any(keep)) {
    return(NULL)
  }
  stats::setNames(sid[keep], nm[keep])
}


#' Reshape a wide eolas dataset to long format
#'
#' Melts the dataset's `value_columns` (from the `layout` metadata attached
#' by [eolas_get()] / [eolas_get_local()]) into a measure-name column and a
#' value column. Requires `layout = "wide"` metadata -- see Details for the
#' full refusal list.
#'
#' @details
#' The layout classification (`long`/`wide`/`feature`/`entity`), the id/time
#' columns, the value columns, and (for long tables that stack >1 measure) the
#' measure-name column all come from server-side metadata
#' (`GET /v1/datasets/{name}`), attached as the `eolas_meta` attribute by
#' [eolas_get()] and [eolas_get_local()] -- identically on the live and the
#' bulk-cache path, since both call the same internal metadata plumbing.
#' This function **never** infers a shape from column names like
#' `date`/`period`/`value`; that inference class is exactly why `eolas_plot()`
#' was removed in v1.3.0 (see `NEWS.md`). It also refuses when `layout` is
#' `"feature"` or `"entity"`, or when a geometry column is present.
#'
#' If the wide table's columns carry a per-column `series_id` mapping
#' (attached as the `eolas_columns` attribute -- see [eolas_column_label()]),
#' a `series_id` column is added to the long result mapping each measure back
#' to its original series id (e.g. RBNZ's ~975 column -> series_id mappings).
#'
#' @param x An `eolas_dataset` object from [eolas_get()] / [eolas_get_local()],
#'   with `layout = "wide"` metadata attached.
#' @param names_to Name of the output column holding the former column
#'   names. `NULL` (default) uses the dataset's `measure_name_column`
#'   metadata, falling back to `"measure"`.
#' @param values_to Name of the output value column. Default `"value"`.
#' @param name Dataset identifier, used only to fetch metadata when `x` has
#'   none attached (e.g. after `meta = FALSE`, or a plain data frame read
#'   from disk). Not required when `x` carries `eolas_meta`.
#' @param base_url Override the API base URL (useful for testing).
#' @return A long-format tibble.
#' @export
#' @examples
#' \dontrun{
#' wide <- eolas_get("rbnz_b1_exchange_rates_monthly")
#' long <- eolas_pivot_longer(wide)
#' }
eolas_pivot_longer <- function(x, names_to = NULL, values_to = "value",
                               name = NULL, base_url = EOLAS_BASE_URL) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame (an {.cls eolas_dataset} from {.fn eolas_get} / {.fn eolas_get_local}).")
  }
  meta <- .eolas_reshape_meta(x, name = name, base_url = base_url)
  layout <- .eolas_reshape_guard(x, meta, name = name)
  ds_name <- name %||% attr(x, "eolas_name") %||% "this dataset"

  if (!identical(layout, "wide")) {
    hint <- if (identical(layout, "long")) {
      c("i" = "This dataset is already long -- see {.fn eolas_pivot_wider} to go the other way.")
    } else {
      character(0)
    }
    cli::cli_abort(c(
      "Cannot {.fn eolas_pivot_longer} {.val {ds_name}}: layout is {.val {layout}}, not {.val wide}.",
      hint
    ))
  }

  value_cols <- intersect(.eolas_meta_vec(meta, "value_columns"), names(x))
  if (!length(value_cols)) {
    cli::cli_abort(c(
      "Cannot {.fn eolas_pivot_longer} {.val {ds_name}}: none of the metadata {.field value_columns} are present in {.arg x}.",
      "i" = "The dataset schema may have changed since metadata was cached; try {.code force = TRUE} on the fetch call."
    ))
  }
  id_cols <- setdiff(intersect(.eolas_meta_vec(meta, "id_columns"), names(x)), value_cols)
  if (!length(id_cols)) {
    id_cols <- setdiff(names(x), value_cols)
  }

  measure_col <- names_to %||% .eolas_first(.eolas_meta_vec(meta, "measure_name_column")) %||% "measure"

  plain <- tibble::as_tibble(x)
  out <- tidyr::pivot_longer(
    plain,
    cols = tidyselect::all_of(value_cols),
    names_to = measure_col,
    values_to = values_to,
    values_drop_na = TRUE
  )

  sid_map <- .eolas_series_id_map(x)
  if (!is.null(sid_map) && !"series_id" %in% names(out)) {
    out$series_id <- unname(sid_map[out[[measure_col]]])
  }

  # Result is now long -- re-declare it so eolas_pivot_wider() can reverse it.
  .eolas_attach_reshaped_meta(
    out, x,
    layout = "long",
    time_cols = .eolas_meta_vec(meta, "time_columns"),
    id_cols = id_cols,
    value_cols = values_to,
    measure_col = measure_col
  )
}


#' Reshape a long eolas dataset to wide format
#'
#' Spreads one long-format value column into multiple columns keyed by a
#' "names" column. Requires `layout = "long"` metadata -- see Details for
#' the full refusal list.
#'
#' @details
#' Unlike [eolas_pivot_longer()], this never has a bare "always safe"
#' default: a long table with more than one dimension column can legitimately
#' be pivoted on several different keys, and picking the wrong one silently
#' produces a cartesian-product blow-up (one row per id-combination x
#' measure, mostly `NA`). `names_from`/`values_from` must therefore be either
#' supplied explicitly, or unambiguously derivable -- a single
#' `value_columns` entry plus a `measure_name_column` in the attached
#' metadata.
#'
#' Also refuses when `layout` metadata is missing, when `layout` is
#' `"feature"`/`"entity"`, or when a geometry column is present -- see
#' [eolas_pivot_longer()].
#'
#' @param x An `eolas_dataset` object, with `layout = "long"` metadata
#'   attached.
#' @param names_from Column to spread into new column names. `NULL`
#'   (default) derives it from the dataset's `measure_name_column`
#'   metadata -- errors if that is unset and `names_from` was not supplied.
#' @param values_from Column to fill the new wide columns with. `NULL`
#'   (default) derives it from `value_columns` metadata -- errors if that
#'   list has more than one entry and `values_from` was not supplied.
#' @param id_cols Columns to keep as row keys. `NULL` (default) uses the
#'   dataset's `id_columns` metadata intersected with the data's columns,
#'   falling back to every column not in `names_from`/`values_from`.
#' @param name Dataset identifier, used only to fetch metadata when `x` has
#'   none attached.
#' @param base_url Override the API base URL (useful for testing).
#' @param ... Forwarded to [tidyr::pivot_wider()] (e.g. `values_fill`).
#' @return A wide-format tibble.
#' @export
#' @examples
#' \dontrun{
#' long <- eolas_get("some_long_series_table")
#' wide <- eolas_pivot_wider(long)
#' }
eolas_pivot_wider <- function(x, names_from = NULL, values_from = NULL,
                              id_cols = NULL, name = NULL,
                              base_url = EOLAS_BASE_URL, ...) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame (an {.cls eolas_dataset} from {.fn eolas_get} / {.fn eolas_get_local}).")
  }
  meta <- .eolas_reshape_meta(x, name = name, base_url = base_url)
  layout <- .eolas_reshape_guard(x, meta, name = name)
  ds_name <- name %||% attr(x, "eolas_name") %||% "this dataset"

  if (!identical(layout, "long")) {
    hint <- if (identical(layout, "wide")) {
      c("i" = "This dataset is already wide -- see {.fn eolas_pivot_longer} to go the other way.")
    } else {
      character(0)
    }
    cli::cli_abort(c(
      "Cannot {.fn eolas_pivot_wider} {.val {ds_name}}: layout is {.val {layout}}, not {.val long}.",
      hint
    ))
  }

  value_cols_meta <- intersect(.eolas_meta_vec(meta, "value_columns"), names(x))

  if (is.null(names_from) || is.null(values_from)) {
    derived_names <- names_from %||% .eolas_first(.eolas_meta_vec(meta, "measure_name_column"))
    derived_values <- values_from %||% (if (length(value_cols_meta) == 1L) value_cols_meta[[1]] else NULL)

    if (is.null(derived_names) || is.null(derived_values)) {
      cli::cli_abort(c(
        "Cannot {.fn eolas_pivot_wider} {.val {ds_name}}: {.arg names_from}/{.arg values_from} were not supplied and cannot be derived unambiguously.",
        "i" = "Metadata has {.field measure_name_column} = {.val {derived_names %||% 'NA'}} and {.field value_columns} = {.val {value_cols_meta}}.",
        "i" = "Supply both explicitly: {.code eolas_pivot_wider(x, names_from = ..., values_from = ...)}."
      ))
    }
    names_from <- names_from %||% derived_names
    values_from <- values_from %||% derived_values
  }

  if (!names_from %in% names(x)) {
    cli::cli_abort("Cannot {.fn eolas_pivot_wider} {.val {ds_name}}: {.arg names_from} column {.val {names_from}} not found in {.arg x}.")
  }
  if (!values_from %in% names(x)) {
    cli::cli_abort("Cannot {.fn eolas_pivot_wider} {.val {ds_name}}: {.arg values_from} column {.val {values_from}} not found in {.arg x}.")
  }

  if (is.null(id_cols)) {
    id_cols <- setdiff(
      intersect(.eolas_meta_vec(meta, "id_columns"), names(x)),
      c(names_from, values_from)
    )
    if (!length(id_cols)) {
      id_cols <- setdiff(names(x), c(names_from, values_from))
    }
  }

  plain <- tibble::as_tibble(x)

  # Refuse non-unique (id_cols + names_from) keys. tidyr would silently produce
  # list-columns + a warning; Python's pivot_wider raises. Match Python (hard
  # error) so the two clients behave identically -- unless the caller opts into
  # aggregation by passing `values_fn` through `...`.
  if (!("values_fn" %in% ...names())) {
    key <- plain[, c(id_cols, names_from), drop = FALSE]
    if (anyDuplicated(key) > 0L) {
      cli::cli_abort(c(
        "Cannot {.fn eolas_pivot_wider} {.val {ds_name}}: {.arg id_cols} + {.arg names_from} do not uniquely identify rows.",
        "i" = "Duplicate keys would collapse into list-columns. Supply a finer {.arg id_cols}, a different {.arg names_from}, or pass {.arg values_fn} to aggregate deliberately."
      ))
    }
  }

  out <- tidyr::pivot_wider(
    plain,
    id_cols = tidyselect::all_of(id_cols),
    names_from = tidyselect::all_of(names_from),
    values_from = tidyselect::all_of(values_from),
    ...
  )

  # Result is now wide -- re-declare it so eolas_pivot_longer() can reverse it.
  .eolas_attach_reshaped_meta(
    out, x,
    layout = "wide",
    time_cols = .eolas_meta_vec(meta, "time_columns"),
    id_cols = id_cols,
    value_cols = setdiff(names(out), id_cols),
    measure_col = ""
  )
}
