# CDC merge utilities for the changelog sync (OUT half of CDC).
#
# Pure merge logic, separated from HTTP so it can be unit-tested without the network.
# This is a faithful port of the Python client's eolas_data/cdc.py — the two clients MUST
# reconstruct identical current state from the same feed (the cross-language correctness
# contract). Keep this in lockstep with cdc.py.
#
# Merge algorithm (pk-keyed, seq-ordered):
#   1. sort the change batch by _eolas_seq ascending (enforced for correctness)
#   2. collect the set of pks touched by ANY change row (including deletes)
#   3. drop ALL local rows whose pk matches a touched pk
#   4. append change rows where _eolas_op != "D" (i.e. I/U) — the new state for those pks
#   5. apply current_state_filter (e.g. "is_current = true" for SCD2)
#   6. strip the _eolas_* meta columns before returning
#
# PK rule: key STRICTLY on pk_columns. Geometry columns are never part of the merge key.

# CDC meta-columns the server attaches to every change row; stripped before writing.
.EOLAS_CDC_COLS <- c("_eolas_seq", "_eolas_op", "_eolas_committed_at", "_eolas_snapshot_id")


# NA-safe composite key. Single column -> character vector; multi-column -> the parts joined
# by US (unit separator, 0x1f), which cannot appear in data. NA gets a distinct sentinel so two
# rows that are both NA in a key column match each other but never a real value. The SAME function
# is used on local and change frames so the keys are comparable.
.eolas_pk_key <- function(df, pk_columns) {
  one <- function(col) {
    v <- df[[col]]
    out <- as.character(v)
    out[is.na(v)] <- "NA"
    out
  }
  if (length(pk_columns) == 1L) return(one(pk_columns))
  do.call(paste, c(lapply(pk_columns, one), sep = ""))
}


# Parse a simple "<col> = <value>" filter (the only form the stream registry supports).
# Returns list(col, value) with booleans/integers coerced, or NULL if absent/unparseable.
.eolas_parse_current_state_filter <- function(filter_expr) {
  if (is.null(filter_expr) || length(filter_expr) != 1L || is.na(filter_expr) ||
      !nzchar(trimws(filter_expr))) {
    return(NULL)
  }
  m <- regmatches(filter_expr, regexec("^\\s*(\\w+)\\s*=\\s*(.+?)\\s*$", filter_expr))[[1]]
  if (length(m) != 3L) return(NULL)
  col <- m[2]
  raw <- trimws(m[3])
  value <- if (tolower(raw) == "true") {
    TRUE
  } else if (tolower(raw) == "false") {
    FALSE
  } else {
    n <- suppressWarnings(as.integer(raw))
    if (!is.na(n) && as.character(n) == raw) n else raw
  }
  list(col = col, value = value)
}


#' Apply a current_state_filter to a data frame
#'
#' Keeps only rows matching `<col> = <value>` (e.g. `"is_current = true"`). Silently returns the
#' frame unchanged when the filter is NULL or the column is absent (append-only tables have no
#' is_current). Comparison is type-tolerant: a boolean filter matches logical, integer, or
#' character storage of the column.
#'
#' @param df A data frame.
#' @param filter_expr A `"<col> = <value>"` string, or NULL.
#' @return The filtered data frame (row names reset).
#' @keywords internal
eolas_apply_current_state_filter <- function(df, filter_expr) {
  parsed <- .eolas_parse_current_state_filter(filter_expr)
  if (is.null(parsed) || !(parsed$col %in% names(df))) return(df)
  col <- df[[parsed$col]]
  val <- parsed$value
  keep <- if (is.logical(val)) {
    tolower(as.character(col)) == tolower(as.character(val))
  } else {
    as.character(col) == as.character(val)
  }
  out <- df[!is.na(keep) & keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}


# rbind two frames that should share business columns, tolerating column-set drift (schema
# evolution): union the columns, NA-fill the missing side, align order. Mirrors pandas concat.
.eolas_rbind_union <- function(a, b) {
  if (nrow(a) == 0L) return(b)
  if (nrow(b) == 0L) return(a)
  cols <- union(names(a), names(b))
  for (c in setdiff(cols, names(a))) a[[c]] <- NA
  for (c in setdiff(cols, names(b))) b[[c]] <- NA
  rbind(a[cols], b[cols])
}


#' Merge a batch of change rows into the current local materialised snapshot
#'
#' Faithful port of the Python client's `merge_changes`. Drops local rows for every pk the feed
#' touched (including deletes), appends the non-delete change rows, applies the
#' `current_state_filter`, and strips the `_eolas_*` meta columns. The result is the current state
#' for the touched pks merged with the untouched local rows.
#'
#' @param local_df Current local materialised state (may be a 0-row frame with the right columns).
#' @param changes_df Change rows from the `/changes` feed. Must contain `_eolas_seq` and `_eolas_op`.
#' @param pk_columns Character vector of primary-key columns (non-empty). The merge keys on these
#'   only — never on geometry.
#' @param current_state_filter Optional `"<col> = <value>"` filter applied after the merge.
#' @return The merged current-state data frame, meta columns stripped, row names reset.
#' @export
eolas_merge_changes <- function(local_df, changes_df, pk_columns, current_state_filter = NULL) {
  if (length(pk_columns) == 0L) {
    cli::cli_abort("{.arg pk_columns} must be a non-empty character vector.")
  }
  missing_cdc <- setdiff(c("_eolas_seq", "_eolas_op"), names(changes_df))
  if (length(missing_cdc)) {
    cli::cli_abort("{.arg changes_df} is missing required CDC column{?s}: {.field {missing_cdc}}.")
  }

  # 1. enforce seq order (server guarantees it; a U is a D then I in ascending seq).
  changes_sorted <- changes_df[order(changes_df[["_eolas_seq"]], method = "radix"), , drop = FALSE]

  # 2. pks touched by ANY change row (incl deletes).
  touched <- unique(.eolas_pk_key(changes_sorted, pk_columns))

  # 3. drop local rows whose pk is touched.
  if (nrow(local_df) && length(touched) && all(pk_columns %in% names(local_df))) {
    surviving <- local_df[!(.eolas_pk_key(local_df, pk_columns) %in% touched), , drop = FALSE]
  } else {
    surviving <- local_df
  }

  # 4. non-delete change rows are the new state; strip meta cols from both sides.
  insertions <- changes_sorted[changes_sorted[["_eolas_op"]] != "D", , drop = FALSE]
  insertions <- insertions[, setdiff(names(insertions), .EOLAS_CDC_COLS), drop = FALSE]
  surviving  <- surviving[, setdiff(names(surviving), .EOLAS_CDC_COLS), drop = FALSE]

  # 5. concat, then 6. filter to current state.
  merged <- .eolas_rbind_union(surviving, insertions)
  merged <- eolas_apply_current_state_filter(merged, current_state_filter)
  rownames(merged) <- NULL
  merged
}
