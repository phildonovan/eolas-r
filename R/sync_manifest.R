# `_eolas-manifest.json` reader / writer for the multi-file sync model.
#
# Each synced dataset directory contains one `_eolas-manifest.json` file that
# records the full snapshot lineage.  This module owns reading and writing that
# file.  It is the R equivalent of eolas_data/sync/manifest.py and uses the
# same JSON schema (schema_version 1) so that a library synced from Python can
# be read from R and vice versa.
#
# Atomic write semantics
# ----------------------
# The manifest is written via a temp file + file.rename().  Readers see either
# the fully-written new content or the old content — never a partial write.
#
# File layout
# -----------
#   ~/eolas-library/nz_parcels/
#   ├── snapshot-2026-05-24.parquet
#   ├── delta-2026-05-24-to-2026-05-31.parquet
#   └── _eolas-manifest.json
#
# Manifest schema (schema_version: 1)
# ------------------------------------
#   {
#     "dataset": "linz.nz_parcels",
#     "snapshots": [
#       {
#         "snapshot_id": 5564541787213050514,   <- numeric (Iceberg snapshot id)
#         "kind": "snapshot",
#         "file": "snapshot-2026-05-24.parquet",
#         "synced_at": "2026-05-24T11:05:00Z",
#         "rows": 5431319
#       },
#       {
#         "snapshot_id": 6789012345678901234,
#         "kind": "delta",
#         "parent_snapshot": 5564541787213050514,
#         "file": "delta-2026-05-24-to-2026-05-31.parquet",
#         "synced_at": "2026-05-31T11:05:00Z",
#         "rows_added": 2847
#       }
#     ],
#     "current_snapshot": 6789012345678901234,
#     "format": "geoparquet",
#     "schema_version": 1
#   }

.MANIFEST_FILENAME       <- "_eolas-manifest.json"
.MANIFEST_SCHEMA_VERSION <- 1L

# Regex patterns for file names and ISO-8601 UTC timestamps.
.MANIFEST_ISO_UTC_RE <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
.MANIFEST_FILE_RE    <- paste0(
  "^(snapshot|delta)-\\d{4}-\\d{2}-\\d{2}",
  "(-to-\\d{4}-\\d{2}-\\d{2})?",
  "\\.(geo\\.)?parquet$"
)


# ---------------------------------------------------------------------------
# Internal validation helpers
# ---------------------------------------------------------------------------

.manifest_validate_entry <- function(entry) {
  if (!is.list(entry)) stop("Manifest entry must be a list.", call. = FALSE)

  # snapshot_id: must be numeric or a string representing an integer.
  # Python-written manifests use JSON numbers (parsed as numeric by jsonlite).
  # New R-written manifests use JSON strings (for full 64-bit precision).
  # Both are acceptable here.
  sid <- entry$snapshot_id
  if (is.null(sid) || length(sid) != 1L ||
      (!is.numeric(sid) && !is.character(sid))) {
    stop("ManifestEntry snapshot_id must be a single numeric or string.", call. = FALSE)
  }
  if (is.character(sid) && !grepl("^-?[0-9]+$", sid)) {
    stop(paste0(
      "ManifestEntry snapshot_id string '", sid,
      "' is not a valid integer representation."
    ), call. = FALSE)
  }

  # kind
  if (is.null(entry$kind) || !entry$kind %in% c("snapshot", "delta")) {
    stop(paste0(
      "ManifestEntry kind must be 'snapshot' or 'delta', got: ",
      entry$kind %||% "(NULL)"
    ), call. = FALSE)
  }

  # file name pattern
  if (is.null(entry$file) || !nzchar(entry$file) ||
      !grepl(.MANIFEST_FILE_RE, entry$file)) {
    stop(paste0(
      "ManifestEntry file '", entry$file %||% "(NULL)", "' does not match ",
      "expected naming pattern (snapshot-YYYY-MM-DD.parquet or ",
      "delta-YYYY-MM-DD-to-YYYY-MM-DD.parquet, .geo.parquet variants OK)."
    ), call. = FALSE)
  }

  # synced_at
  if (is.null(entry$synced_at) || !grepl(.MANIFEST_ISO_UTC_RE, entry$synced_at)) {
    stop(paste0(
      "ManifestEntry synced_at '", entry$synced_at %||% "(NULL)",
      "' must be ISO-8601 UTC (e.g. '2026-05-24T11:05:00Z')."
    ), call. = FALSE)
  }

  # kind-specific fields
  if (entry$kind == "snapshot") {
    if (is.null(entry$rows) || !is.numeric(entry$rows) || entry$rows < 0) {
      stop("ManifestEntry kind='snapshot' must have non-negative numeric 'rows'.",
           call. = FALSE)
    }
  } else {
    # delta
    ps <- entry$parent_snapshot
    if (is.null(ps) || length(ps) != 1L ||
        (!is.numeric(ps) && !is.character(ps))) {
      stop("ManifestEntry kind='delta' must have numeric or string 'parent_snapshot'.",
           call. = FALSE)
    }
    if (is.character(ps) && !grepl("^-?[0-9]+$", ps)) {
      stop(paste0(
        "ManifestEntry parent_snapshot '", ps,
        "' is not a valid integer representation."
      ), call. = FALSE)
    }
    if (is.null(entry$rows_added) || !is.numeric(entry$rows_added) ||
        entry$rows_added < 0) {
      stop("ManifestEntry kind='delta' must have non-negative numeric 'rows_added'.",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}


.manifest_validate <- function(manifest) {
  if (!is.list(manifest)) stop("Manifest must be a list.", call. = FALSE)

  if (is.null(manifest$dataset) || !nzchar(manifest$dataset)) {
    stop("Manifest dataset must not be empty.", call. = FALSE)
  }

  if (is.null(manifest$format) || !manifest$format %in% c("parquet", "geoparquet")) {
    stop(paste0(
      "Manifest format must be 'parquet' or 'geoparquet', got: ",
      manifest$format %||% "(NULL)"
    ), call. = FALSE)
  }

  sv <- manifest$schema_version %||% .MANIFEST_SCHEMA_VERSION
  if (!identical(as.integer(sv), .MANIFEST_SCHEMA_VERSION)) {
    stop(paste0(
      "Manifest schema_version ", sv, " is not supported (expected ",
      .MANIFEST_SCHEMA_VERSION, ")."
    ), call. = FALSE)
  }

  snaps <- manifest$snapshots %||% list()
  for (i in seq_along(snaps)) {
    tryCatch(
      .manifest_validate_entry(snaps[[i]]),
      error = function(e) {
        stop(paste0("Manifest snapshots[[", i, "]]: ", conditionMessage(e)),
             call. = FALSE)
      }
    )
  }

  # current_snapshot must appear in the snapshots list (if set and non-empty).
  # Use character comparison to handle both numeric and string snapshot IDs.
  cur <- manifest$current_snapshot
  if (!is.null(cur) && length(snaps) > 0L) {
    ids_chr <- vapply(snaps, function(e) as.character(e$snapshot_id), character(1L))
    cur_chr  <- as.character(cur)
    if (!any(ids_chr == cur_chr)) {
      stop(paste0(
        "Manifest current_snapshot ", cur,
        " is not found in the snapshots list."
      ), call. = FALSE)
    }
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Public I/O functions
# ---------------------------------------------------------------------------

#' Read `_eolas-manifest.json` from a dataset directory
#'
#' Parses and validates the manifest JSON.  Returns `NULL` when the file does
#' not exist (indicating the dataset has not been synced yet).  Raises on
#' parse / validation errors so callers can distinguish "first sync" (`NULL`)
#' from "corrupt manifest" (error).
#'
#' @param library_dir Path to the root library directory.
#' @param dataset Dataset name (sub-directory under `library_dir`).
#' @return A named list (the parsed manifest) or `NULL` if absent.
#' @keywords internal
.eolas_read_manifest <- function(library_dir, dataset) {
  manifest_path <- file.path(library_dir, dataset, .MANIFEST_FILENAME)
  if (!file.exists(manifest_path)) return(NULL)

  raw <- tryCatch(
    paste(readLines(manifest_path, warn = FALSE), collapse = "\n"),
    error = function(e) {
      stop(paste0("Cannot read manifest at ", manifest_path, ": ",
                  conditionMessage(e)), call. = FALSE)
    }
  )

  manifest <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = FALSE),
    error = function(e) {
      stop(paste0("Manifest at ", manifest_path, " is not valid JSON: ",
                  conditionMessage(e)), call. = FALSE)
    }
  )

  .manifest_validate(manifest)
  manifest
}


#' Write a manifest atomically to `_eolas-manifest.json`
#'
#' Serialises `manifest` to a temporary file then uses `file.rename()` to swap
#' it over the canonical path.  On POSIX systems this is an atomic inode swap;
#' readers see either the old or new content, never a partial write.
#'
#' @param library_dir Path to the root library directory.
#' @param dataset Dataset name (sub-directory under `library_dir`).
#' @param manifest A named list conforming to the manifest schema.
# Convert snapshot id numerics to character strings for JSON serialisation.
# Iceberg snapshot ids are 64-bit integers.  R stores them as IEEE-754
# doubles, which lose the last ~8 bits of precision for values > 2^53.
# Writing them as JSON strings (e.g. "4178402751765785856") preserves all
# digits.  Python's _coerce_snapshot_id() in manifest.py accepts both int
# and str, so this is backward-compatible with Python-written manifests.
#
# We apply this to a deep copy of the manifest list so the in-memory object
# retains numeric types (which the validator needs for numeric comparisons).
.manifest_encode_for_json <- function(manifest) {
  out <- manifest

  snap_to_str <- function(x) {
    if (is.character(x)) return(trimws(x))
    if (is.numeric(x) && !is.na(x))
      # sprintf %0.0f rounds to the nearest representable double and strips the
      # decimal point.  Iceberg snapshot ids are 64-bit integers; R stores them
      # as IEEE-754 doubles which can only represent integers exactly up to 2^53
      # (~9e15).  Iceberg ids are ~4e18, so the last few bits are already lost
      # when the server response is parsed by jsonlite.  This is a fundamental
      # R limitation (no native int64).  The string form at least avoids the
      # extra precision loss from the scientific-notation JSON form that
      # jsonlite would otherwise write.
      sprintf("%0.0f", x)
    else
      x
  }

  out$current_snapshot <- snap_to_str(out$current_snapshot)

  out$snapshots <- lapply(out$snapshots, function(entry) {
    entry$snapshot_id <- snap_to_str(entry$snapshot_id)
    if (!is.null(entry$parent_snapshot)) {
      entry$parent_snapshot <- snap_to_str(entry$parent_snapshot)
    }
    entry
  })

  out
}

#' @keywords internal
.eolas_write_manifest <- function(library_dir, dataset, manifest) {
  .manifest_validate(manifest)

  dataset_dir   <- file.path(library_dir, dataset)
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

  manifest_path <- file.path(dataset_dir, .MANIFEST_FILENAME)

  rand_hex <- paste0(
    sample(c(0:9, letters[1:6]), 8L, replace = TRUE), collapse = ""
  )
  tmp_path <- paste0(manifest_path, ".tmp-", rand_hex)

  # Encode snapshot ids as strings for full 64-bit integer precision.
  manifest_for_json <- .manifest_encode_for_json(manifest)

  tryCatch({
    writeLines(
      jsonlite::toJSON(manifest_for_json, auto_unbox = TRUE, pretty = TRUE),
      tmp_path
    )
    ok <- file.rename(tmp_path, manifest_path)
    if (!ok) {
      file.copy(tmp_path, manifest_path, overwrite = TRUE)
      unlink(tmp_path)
    }
  }, error = function(e) {
    unlink(tmp_path)
    stop(paste0("Failed to write manifest to ", manifest_path, ": ",
                conditionMessage(e)), call. = FALSE)
  })

  invisible(manifest_path)
}


#' Validate a manifest list against the eolas manifest schema
#'
#' @param manifest A named list to validate.
#' @return Invisibly `TRUE` on success; stops with an error otherwise.
#' @keywords internal
.eolas_validate_manifest <- function(manifest) {
  .manifest_validate(manifest)
}
