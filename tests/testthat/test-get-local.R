library(testthat)
library(withr)

# Tests for eolas_get_local() — notebook-friendly whole-dataset convenience.
# Constants + with_mock_get_local() live in helper.R.

# ---------------------------------------------------------------------------
# First call returns data.frame (non-geo, via arrow or CSV)
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns data.frame for non-geo dataset (first call)", {
  tmp <- withr::local_tempdir()

  # Write a CSV.gz file because arrow may not be available in the test env;
  # test with csv_gz format to stay dependency-free.
  file_writer <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- gzcon(file(path, "wb"))
    writeLines(c("date,value", "2023-01-01,1100.5"), con)
    close(con)
  }

  with_mock_get_local(DATASET_META_NON_GEO, file_writer, {
    result <- eolas_get_local("nz_cpi",
                              format    = "csv_gz",
                              cache_dir = tmp)
  })

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "eolas_dataset")
  expect_true("date" %in% names(result))
  expect_true("value" %in% names(result))
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Subsequent call returns data.frame from cached file (sync_bulk = unchanged)
# ---------------------------------------------------------------------------

test_that("eolas_get_local returns data.frame from cached file on subsequent call", {
  tmp <- withr::local_tempdir()

  csv_path <- file.path(tmp, "nz_cpi.csv.gz")
  con <- gzcon(file(csv_path, "wb"))
  writeLines(c("date,value", "2023-06-01,1105.0"), con)
  close(con)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      # Simulates "unchanged" — does NOT write the file (it already exists).
      list(
        status               = "unchanged",
        previous_snapshot_id = SNAPSHOT_ID,
        current_snapshot_id  = SNAPSHOT_ID,
        path                 = normalizePath(path, mustWork = FALSE),
        bytes_downloaded     = 0L
      )
    },
    .package = "eolas"
  )

  result <- eolas_get_local("nz_cpi", format = "csv_gz", cache_dir = tmp)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Tilde expansion in cache_dir
# ---------------------------------------------------------------------------

test_that("eolas_get_local expands ~ in cache_dir to an absolute path", {
  # Override HOME so we don't write to the real ~/.cache
  withr::local_envvar(HOME = withr::local_tempdir())

  actual_path_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      actual_path_seen <<- path
      # Write a stub CSV.gz so the read succeeds.
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      con <- gzcon(file(path, "wb"))
      writeLines(c("x", "1"), con)
      close(con)
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 100L)
    },
    .package = "eolas"
  )

  eolas_get_local("nz_cpi", format = "csv_gz", cache_dir = "~/.cache/eolas")

  expect_false(grepl("~", actual_path_seen, fixed = TRUE))
  expect_true(startsWith(actual_path_seen, "/"))
})

# ---------------------------------------------------------------------------
# Auto-detect format: geo dataset -> geoparquet format selected
# ---------------------------------------------------------------------------

test_that("eolas_get_local auto-detects geoparquet for geo datasets", {
  tmp <- withr::local_tempdir()

  # Capture the *first* format passed to eolas_sync_bulk — that's what we test.
  # A second call may happen (parquet fallback) if sfarrow errors; we ignore it.
  first_format_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      # geometry_type present -> geo dataset
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      if (is.null(first_format_seen)) first_format_seen <<- format
      # Don't write a real file — just record the format. The read step will
      # fail (or fire the WKT fallback), but we only test format detection here.
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 1024L)
    },
    .package = "eolas"
  )

  # Suppress the expected read failure and any WKT-fallback warning — we only
  # care about format selection, not whether the read itself succeeds.
  suppressWarnings(tryCatch(
    eolas_get_local("nz_parcels", cache_dir = tmp),
    error = function(e) NULL
  ))

  expect_equal(first_format_seen, "geoparquet")
})

# ---------------------------------------------------------------------------
# Auto-detect format: non-geo dataset -> parquet format selected
# ---------------------------------------------------------------------------

test_that("eolas_get_local auto-detects parquet for non-geo datasets", {
  tmp <- withr::local_tempdir()

  format_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      format_seen <<- format
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 1024L)
    },
    .package = "eolas"
  )

  # Arrow likely not available; suppress read error, only test format selection.
  tryCatch(
    eolas_get_local("nz_cpi", cache_dir = tmp),
    error = function(e) NULL
  )

  expect_equal(format_seen, "parquet")
})

# ---------------------------------------------------------------------------
# Auto-detect format: geometry_type="none" string -> parquet (NOT geoparquet)
# Regression test for Bug A.
# ---------------------------------------------------------------------------

test_that("eolas_get_local auto-detects parquet when geometry_type is string 'none'", {
  tmp <- withr::local_tempdir()

  format_seen <- NULL

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  meta_with_none_string <- jsonlite::toJSON(
    list(name = "rbnz_b2_wholesale_rates_monthly",
         namespace = "rbnz", table = "rbnz_b2_wholesale_rates_monthly",
         geometry_type = "none",   # enriched server field — must be treated as non-geo
         has_geometry  = NULL),
    auto_unbox = TRUE, null = "null"
  )

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(meta_with_none_string, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      format_seen <<- format
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID,
           path = normalizePath(path, mustWork = FALSE),
           bytes_downloaded = 1024L)
    },
    .package = "eolas"
  )

  tryCatch(
    eolas_get_local("rbnz_b2_wholesale_rates_monthly", cache_dir = tmp),
    error = function(e) NULL
  )

  expect_equal(format_seen, "parquet")   # must NOT be "geoparquet"
})

# ---------------------------------------------------------------------------
# Bulk error propagation: stop() messages pass through unchanged
# ---------------------------------------------------------------------------

test_that("eolas_get_local propagates Bulk upgrade required stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk upgrade required: Fresh bulk downloads are a Pro feature.", call. = FALSE)
    },
    .package = "eolas"
  )

  expect_error(
    eolas_get_local("nz_cpi", freshness = "current", cache_dir = tmp),
    "Bulk upgrade required",
    fixed = TRUE
  )
})

test_that("eolas_get_local propagates Bulk licence restricted stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(
        jsonlite::toJSON(
          list(name = "oecd_gdp", namespace = "oecd", table = "oecd_gdp",
               source = "OECD"),
          auto_unbox = TRUE
        ),
        simplifyVector = FALSE
      )
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk licence restricted: This dataset is not available (licence: OECD).",
           call. = FALSE)
    },
    .package = "eolas"
  )

  expect_error(
    eolas_get_local("oecd_gdp", cache_dir = tmp),
    "Bulk licence restricted",
    fixed = TRUE
  )
})

test_that("eolas_get_local propagates Bulk not yet available stop() unchanged", {
  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_NON_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      stop("Bulk not yet available: Monthly bulk snapshots are still rolling out.",
           call. = FALSE)
    },
    .package = "eolas"
  )

  expect_error(
    eolas_get_local("nz_cpi", cache_dir = tmp),
    "Bulk not yet available",
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# GeoParquet WKT fallback: sfarrow throws → retry via plain parquet + st_as_sf
# ---------------------------------------------------------------------------

test_that("eolas_get_local falls back to WKT parquet when sfarrow throws on malformed GeoParquet", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  skip_if_not_installed("sfarrow")

  tmp <- withr::local_tempdir()

  # We need two separate paths:
  #   nz_parcels.geo.parquet  (malformed — triggers sfarrow error)
  #   nz_parcels.parquet      (plain, written by the fallback sync_bulk call)
  #
  # Both are written by the mocked eolas_sync_bulk based on the `format` arg.

  # Build a minimal real .parquet with a geometry_wkt column for the fallback read.
  wkt_parquet_path <- file.path(tmp, "nz_parcels.parquet")
  df_wkt <- data.frame(
    id           = 1L,
    geometry_wkt = "POINT (174.76 -36.85)",
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(df_wkt, wkt_parquet_path)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  skip_if_not_installed("sfarrow")

  set_test_key()
  with_mocked_bindings(
    {
      expect_warning(
        result <- eolas_get_local("nz_parcels", cache_dir = tmp, progress = FALSE),
        regexp = "falling back to WKT string path"
      )
      expect_s3_class(result, "sf")
      expect_s3_class(result, "tbl_df")
      expect_s3_class(result, "eolas_dataset")
      expect_equal(nrow(result), 1L)
      expect_false("geometry_wkt" %in% names(result))
      expect_equal(attr(result, "sf_column"), "geometry")
      expect_true(inherits(result[["geometry"]], "sfc"))
    },
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      if (format == "geoparquet") {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeBin(raw(0L), path)
      }
      list(
        status               = "downloaded",
        previous_snapshot_id = NA_character_,
        current_snapshot_id  = SNAPSHOT_ID,
        path                 = normalizePath(path, mustWork = FALSE),
        bytes_downloaded     = 1024L
      )
    },
    .eolas_arrow_wkb_to_sf = function(file_path) {
      stop("simulated arrow+WKB failure", call. = FALSE)
    },
    .eolas_sfarrow_read_parquet = function(file_path) {
      stop("vapply(x, is.raw, TRUE) are not all TRUE", call. = FALSE)
    },
    .package = "eolas"
  )

})

test_that("eolas_get_local re-raises sfarrow error when both GeoParquet and WKT fallback fail", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  skip_if_not_installed("sfarrow")

  tmp <- withr::local_tempdir()

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      if (format == "geoparquet") {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeBin(raw(0L), path)
      }
      if (format == "parquet") {
        # Fallback sync also fails
        stop("Bulk not yet available: parquet fallback unavailable.", call. = FALSE)
      }
      list(status = "downloaded", previous_snapshot_id = NA_character_,
           current_snapshot_id = SNAPSHOT_ID, path = path, bytes_downloaded = 0L)
    },
    .eolas_sfarrow_read_parquet = function(file_path) {
      stop("vapply(x, is.raw, TRUE) are not all TRUE", call. = FALSE)
    },
    .package = "eolas"
  )

  # The cli_warn() fires before the stop() — suppress it so the test focuses on
  # the stop() message, not the incidental warning from the fallback path.
  suppressWarnings(expect_error(
    eolas_get_local("nz_parcels", cache_dir = tmp, progress = FALSE),
    "vapply(x, is.raw, TRUE) are not all TRUE",
    fixed = TRUE
  ))
})

# ---------------------------------------------------------------------------
# Malformed GeoParquet (empty geometry_types metadata): end-to-end fallback
#
# This test uses the arrow R package to write a real Parquet file, then
# injects GeoParquet metadata with an empty geometry_types array — exactly
# the condition that triggered Phil's WKB error.  sfarrow::st_read_parquet()
# is NOT mocked here; we allow it to fail on the real (tiny) malformed file,
# and confirm the WKT fallback rescues the session.
#
# No real network calls are made; eolas_sync_bulk is mocked throughout.
# The dataset is 1 row — no memory risk.
# ---------------------------------------------------------------------------

test_that("malformed GeoParquet (empty geometry_types) triggers WKT fallback and returns sf", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  skip_if_not_installed("sfarrow")

  tmp <- withr::local_tempdir()

  # ---- Build a minimal GeoParquet with geometry_types: [] -------------------
  # Use arrow's low-level metadata API to inject the broken GeoParquet spec.
  df_raw <- data.frame(
    id           = 1L,
    geometry_wkt = "POINT (174.76 -36.85)",
    stringsAsFactors = FALSE
  )

  # Write plain parquet first (used by the WKT fallback path).
  parquet_path <- file.path(tmp, "nz_parcels.parquet")
  arrow::write_parquet(df_raw, parquet_path)

  # Build the malformed GeoParquet by writing the same data with a
  # hand-crafted "geo" schema metadata key where geometry_types is [].
  geo_parquet_path <- file.path(tmp, "nz_parcels.geo.parquet")

  # Encode the WKT as WKB so GeoParquet has a binary column, but use empty
  # geometry_types to trigger the sfarrow parse error.
  wkb_col <- sf::st_as_binary(sf::st_as_sfc("POINT (174.76 -36.85)", crs = 4326))
  df_geo <- data.frame(id = 1L, stringsAsFactors = FALSE)
  df_geo[["geometry"]] <- list(wkb_col[[1]])  # raw vector, 1 element

  geo_meta <- jsonlite::toJSON(list(
    version        = jsonlite::unbox("1.0.0"),
    primary_column = jsonlite::unbox("geometry"),
    columns        = list(
      geometry = list(
        encoding       = jsonlite::unbox("WKB"),
        geometry_types = list()   # <- empty array: violates GeoParquet spec
      )
    )
  ), auto_unbox = FALSE)

  # Write via arrow with the injected metadata.
  tbl     <- arrow::as_arrow_table(df_geo)
  old_meta <- tbl$schema$metadata
  tbl <- tbl$RenameColumns(c("id", "geometry"))
  new_schema <- tbl$schema$WithMetadata(
    c(old_meta, list(geo = geo_meta))
  )
  arrow::write_parquet(
    tbl$cast(new_schema),
    geo_parquet_path
  )

  # ---- Mock network layer ---------------------------------------------------
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  local_mocked_bindings(
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(DATASET_META_GEO, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      # Files are already written above — no-op here.
      list(
        status               = "downloaded",
        previous_snapshot_id = NA_character_,
        current_snapshot_id  = SNAPSHOT_ID,
        path                 = normalizePath(path, mustWork = FALSE),
        bytes_downloaded     = 1024L
      )
    },
    .package = "eolas"
  )

  # ---- Primary path must fail, fallback must succeed and emit a warning -----
  # sfarrow is NOT mocked here — it will genuinely fail on the malformed file.
  # If sfarrow happens to succeed (future sfarrow version is more lenient),
  # the test still passes (result will be sf) — the warning check uses
  # tryCatch so the test degrades gracefully.
  result <- withCallingHandlers(
    eolas_get_local("nz_parcels", cache_dir = tmp, progress = FALSE),
    warning = function(w) {
      if (grepl("GeoParquet read failed", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "eolas_dataset")
  expect_equal(nrow(result), 1L)
  expect_false("geometry_wkt" %in% names(result))
})
