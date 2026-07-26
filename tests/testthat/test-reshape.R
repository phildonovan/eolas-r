library(testthat)

# Tests for eolas_pivot_longer() / eolas_pivot_wider() -- the long/wide
# reshape helpers (2026-07-27 contract, see
# eolas/docs/long-format-toggle-plan-2026-07-27.md). Mirrors
# eolas-data/tests/test_reshape.py.

# ---------------------------------------------------------------------------
# Fixtures -- a wide RBNZ-style FX table and its long equivalent
# ---------------------------------------------------------------------------

WIDE_META_JSON <- jsonlite::toJSON(
  list(
    name = "rbnz_fx_test", title = "Test FX rates (wide)", source = "RBNZ",
    namespace = "rbnz", table = "rbnz_fx_test",
    layout = "wide",
    time_columns = list("date"),
    id_columns = list("date"),
    value_columns = list("usd", "aud", "eur"),
    columns = list(
      list(name = "date", type = "date", description = "Observation date"),
      list(name = "usd", type = "double", description = "USD rate", series_id = "RBNZD.SUSD"),
      list(name = "aud", type = "double", description = "AUD rate", series_id = "RBNZD.SAUD"),
      list(name = "eur", type = "double", description = "EUR rate", series_id = "RBNZD.SEUR")
    )
  ),
  auto_unbox = TRUE
)

LONG_META_JSON <- jsonlite::toJSON(
  list(
    name = "rbnz_fx_test_long", title = "Test FX rates (long)", source = "RBNZ",
    namespace = "rbnz", table = "rbnz_fx_test_long",
    layout = "long",
    time_columns = list("date"),
    id_columns = list("date"),
    value_columns = list("value"),
    measure_name_column = "currency"
  ),
  auto_unbox = TRUE
)

.meta_from_json <- function(json) {
  .eolas_parse_info_response(jsonlite::fromJSON(json, simplifyVector = FALSE))
}

wide_frame <- function() {
  df <- tibble::tibble(
    date = c("2023-01-01", "2023-02-01"),
    usd  = c(0.61, 0.62),
    aud  = c(0.93, 0.94),
    eur  = c(0.56, NA)
  )
  new_eolas_dataset(df, name = "rbnz_fx_test", meta_info = .meta_from_json(WIDE_META_JSON))
}

long_frame <- function() {
  df <- tibble::tibble(
    date     = c("2023-01-01", "2023-01-01", "2023-02-01", "2023-02-01"),
    currency = c("usd", "aud", "usd", "aud"),
    value    = c(0.61, 0.93, 0.62, 0.94)
  )
  new_eolas_dataset(df, name = "rbnz_fx_test_long", meta_info = .meta_from_json(LONG_META_JSON))
}

no_layout_meta <- function() {
  .meta_from_json(jsonlite::toJSON(
    list(name = "no_layout_dataset", title = "No layout"),
    auto_unbox = TRUE
  ))
}

layout_only_meta <- function(layout) {
  .meta_from_json(jsonlite::toJSON(
    list(name = "feature_or_entity", layout = layout),
    auto_unbox = TRUE
  ))
}

# ---------------------------------------------------------------------------
# eolas_pivot_longer -- happy path
# ---------------------------------------------------------------------------

test_that("eolas_pivot_longer melts a wide table and preserves series_id", {
  wide <- wide_frame()
  long <- eolas_pivot_longer(wide)

  expect_setequal(names(long), c("date", "measure", "value", "series_id"))
  expect_setequal(unique(long$measure), c("usd", "aud", "eur"))
  # eur/2023-02-01 is NA in the source -- dropped (values_drop_na = TRUE).
  expect_equal(nrow(long), 5L)

  row <- long[long$date == "2023-01-01" & long$measure == "usd", ]
  expect_equal(row$series_id, "RBNZD.SUSD")
})

test_that("eolas_pivot_longer respects an explicit names_to", {
  wide <- wide_frame()
  long <- eolas_pivot_longer(wide, names_to = "currency")
  expect_true("currency" %in% names(long))
  expect_false("measure" %in% names(long))
})

# ---------------------------------------------------------------------------
# eolas_pivot_wider -- happy path
# ---------------------------------------------------------------------------

test_that("eolas_pivot_wider derives names_from/values_from from metadata", {
  long <- long_frame()
  wide <- eolas_pivot_wider(long)
  expect_setequal(names(wide), c("date", "usd", "aud"))
  row <- wide[wide$date == "2023-01-01", ]
  expect_equal(row$usd, 0.61)
  expect_equal(row$aud, 0.93)
})

test_that("eolas_pivot_wider accepts explicit names_from/values_from", {
  long <- long_frame()
  wide <- eolas_pivot_wider(long, names_from = "currency", values_from = "value")
  expect_setequal(names(wide), c("date", "usd", "aud"))
})

test_that("eolas_pivot_longer then eolas_pivot_wider round-trips (meta auto-carried)", {
  wide <- wide_frame()
  long <- eolas_pivot_longer(wide, names_to = "currency")
  # P0 #2 fix: pivot_longer must re-declare the result as long ITSELF, so the
  # reverse pivot works with no hand-re-wrap (the old test masked this by
  # rebuilding the dataset with a fresh long meta).
  expect_equal(attr(long, "eolas_meta")$layout, "long")
  expect_equal(.eolas_first(.eolas_meta_vec(attr(long, "eolas_meta"), "measure_name_column")), "currency")
  back <- eolas_pivot_wider(long)
  expect_setequal(names(back), c("date", "usd", "aud", "eur"))
  # and the re-widened frame re-declares itself wide
  expect_equal(attr(back, "eolas_meta")$layout, "wide")
})

test_that("eolas_pivot_wider refuses non-unique id + names_from keys", {
  meta <- .meta_from_json(jsonlite::toJSON(
    list(
      name = "dup_key_long", layout = "long",
      id_columns = list("date", "area", "measure"),
      value_columns = list("value"), measure_name_column = "measure"
    ),
    auto_unbox = TRUE
  ))
  df <- new_eolas_dataset(
    tibble::tibble(
      date = c("2023-01-01", "2023-01-01"),
      area = c("Auckland", "Auckland"),
      measure = c("Enterprises", "Enterprises"),
      value = c(100, 200)
    ),
    name = "dup_key_long", meta_info = meta
  )
  expect_error(eolas_pivot_wider(df), "do not uniquely identify")
})

test_that("eolas_pivot_wider allows deliberate aggregation via values_fn", {
  meta <- .meta_from_json(jsonlite::toJSON(
    list(
      name = "dup_key_long", layout = "long",
      id_columns = list("date", "area", "measure"),
      value_columns = list("value"), measure_name_column = "measure"
    ),
    auto_unbox = TRUE
  ))
  df <- new_eolas_dataset(
    tibble::tibble(
      date = c("2023-01-01", "2023-01-01"),
      area = c("Auckland", "Auckland"),
      measure = c("Enterprises", "Enterprises"),
      value = c(100, 200)
    ),
    name = "dup_key_long", meta_info = meta
  )
  wide <- eolas_pivot_wider(df, values_fn = sum)
  expect_equal(wide$Enterprises, 300)
})

# ---------------------------------------------------------------------------
# Refuse: no metadata
# ---------------------------------------------------------------------------

test_that("eolas_pivot_longer refuses without metadata", {
  df <- tibble::tibble(date = "2023-01-01", value = 1.0)
  expect_error(eolas_pivot_longer(df), "no eolas layout metadata")
})

test_that("eolas_pivot_wider refuses without metadata", {
  df <- tibble::tibble(date = "2023-01-01", measure = "x", value = 1.0)
  expect_error(eolas_pivot_wider(df), "no eolas layout metadata")
})

test_that("eolas_pivot_longer refuses when layout field is missing but meta is present", {
  df <- new_eolas_dataset(
    tibble::tibble(date = "2023-01-01", value = 1.0),
    name = "no_layout_dataset", meta_info = no_layout_meta()
  )
  expect_error(eolas_pivot_longer(df), "no.*layout.*metadata")
})

# ---------------------------------------------------------------------------
# Refuse: feature / entity layout
# ---------------------------------------------------------------------------

for (lyt in c("feature", "entity")) {
  local({
    layout <- lyt
    test_that(paste0("eolas_pivot_longer refuses layout = ", layout), {
      df <- new_eolas_dataset(
        tibble::tibble(id = 1L, value = 1.0),
        name = "feature_or_entity", meta_info = layout_only_meta(layout)
      )
      expect_error(eolas_pivot_longer(df), paste0("layout is .", layout, "."))
    })
    test_that(paste0("eolas_pivot_wider refuses layout = ", layout), {
      df <- new_eolas_dataset(
        tibble::tibble(id = 1L, value = 1.0),
        name = "feature_or_entity", meta_info = layout_only_meta(layout)
      )
      expect_error(eolas_pivot_wider(df), paste0("layout is .", layout, "."))
    })
  })
}

# ---------------------------------------------------------------------------
# Refuse: geometry present
# ---------------------------------------------------------------------------

test_that("eolas_pivot_longer refuses when geometry_wkt is present", {
  meta <- .meta_from_json(jsonlite::toJSON(
    list(name = "geo_table", layout = "wide", value_columns = list("value")),
    auto_unbox = TRUE
  ))
  df <- new_eolas_dataset(
    tibble::tibble(id = 1L, geometry_wkt = "POINT (1 1)", value = 1.0),
    name = "geo_table", meta_info = meta
  )
  expect_error(eolas_pivot_longer(df), "geometry column is present")
})

test_that("eolas_pivot_wider refuses when geometry_wkt is present", {
  meta <- .meta_from_json(jsonlite::toJSON(
    list(
      name = "geo_table_long", layout = "long",
      measure_name_column = "measure", value_columns = list("value")
    ),
    auto_unbox = TRUE
  ))
  df <- new_eolas_dataset(
    tibble::tibble(id = 1L, geometry_wkt = "POINT (1 1)", measure = "a", value = 1.0),
    name = "geo_table_long", meta_info = meta
  )
  expect_error(eolas_pivot_wider(df), "geometry column is present")
})

# ---------------------------------------------------------------------------
# Refuse: wrong-direction layout / ambiguous columns
# ---------------------------------------------------------------------------

test_that("eolas_pivot_longer refuses an already-long dataset", {
  long <- long_frame()
  expect_error(eolas_pivot_longer(long), "already long")
})

test_that("eolas_pivot_wider refuses an already-wide dataset", {
  wide <- wide_frame()
  expect_error(eolas_pivot_wider(wide), "already wide")
})

test_that("eolas_pivot_wider refuses ambiguous names_from/values_from", {
  meta <- .meta_from_json(jsonlite::toJSON(
    list(
      name = "ambiguous_long", layout = "long",
      id_columns = list("date"), value_columns = list("recipients", "cancels")
    ),
    auto_unbox = TRUE
  ))
  df <- new_eolas_dataset(
    tibble::tibble(date = "2023-01-01", recipients = 10L, cancels = 2L),
    name = "ambiguous_long", meta_info = meta
  )
  expect_error(eolas_pivot_wider(df), "cannot be derived unambiguously")
})

# ---------------------------------------------------------------------------
# Contract parity: live path (eolas_get) vs bulk path (eolas_get_local)
# attach identical layout metadata and pivot identically.
# ---------------------------------------------------------------------------

LIVE_WIDE_META_JSON <- '{"name":"rbnz_fx_live_test","title":"Test FX rates (wide, live)",
  "source":"RBNZ","namespace":"rbnz","table":"rbnz_fx_live_test",
  "layout":"wide","id_columns":["date"],"value_columns":["usd","aud"],
  "columns":[
    {"name":"date","type":"date"},
    {"name":"usd","type":"double","series_id":"RBNZD.SUSD"},
    {"name":"aud","type":"double","series_id":"RBNZD.SAUD"}
  ]}'

LIVE_WIDE_DATA_BODY <- '{"data":[
  {"date":"2023-01-01","usd":0.61,"aud":0.93},
  {"date":"2023-02-01","usd":0.62,"aud":0.94}
]}'

test_that("live path (eolas_get) attaches layout metadata and pivots", {
  set_test_key()
  with_mocked_bindings(
    {
      df <- eolas_get("rbnz_fx_live_test")
      long <- eolas_pivot_longer(df)

      expect_setequal(names(long), c("date", "measure", "value", "series_id"))
      expect_equal(nrow(long), 4L)
      usd_rows <- long[long$measure == "usd", ]
      expect_equal(nrow(usd_rows), 2L)
      expect_true(all(usd_rows$series_id == "RBNZD.SUSD"))
    },
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        httr2_mock_resp(LIVE_WIDE_DATA_BODY)
      } else {
        httr2_mock_resp(LIVE_WIDE_META_JSON)
      }
    },
    .package = "eolas"
  )
})

test_that("bulk path (eolas_get_local) attaches the same layout metadata and pivots to the same shape", {
  # 'date' stays character on this path (the CSV reader doesn't parse dates) --
  # an unrelated, pre-existing asymmetry with the live path's as.Date() coercion
  # -- so this checks structure/series_id, not a byte-for-byte frame diff.
  #
  # Mocks eolas_info()/eolas_sync_bulk() directly (like test-get-local.R),
  # NOT with_mock_get_local() -- that shared helper's eolas_info stub returns
  # fromJSON(simplifyVector = FALSE) raw output rather than the parsed tibble
  # .eolas_parse_info_response() builds, so .eolas_table_meta() attaches
  # nothing (a harness quirk, tracked separately). Returning the real parsed
  # tibble here exercises eolas_get_local()'s actual metadata-attachment code
  # (.eolas_fetch_meta_info -> .eolas_finalize_dataset) for real.
  tmp <- withr::local_tempdir()
  set_test_key()

  file_writer <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- gzcon(file(path, "wb"))
    writeLines(c("date,usd,aud", "2023-01-01,0.61,0.93", "2023-02-01,0.62,0.94"), con)
    close(con)
  }

  bulk_df <- with_mocked_bindings(
    eolas_get_local("rbnz_fx_live_test_bulk", format = "csv_gz", cache_dir = tmp),
    eolas_info = function(n, base_url = NULL) .meta_from_json(LIVE_WIDE_META_JSON),
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      file_writer(path)
      fake_sync_result(path)
    },
    .package = "eolas"
  )

  expect_false(is.null(attr(bulk_df, "eolas_meta")))

  long_bulk <- eolas_pivot_longer(bulk_df)

  expect_setequal(names(long_bulk), c("date", "measure", "value", "series_id"))
  expect_equal(nrow(long_bulk), 4L)
  usd_rows <- long_bulk[long_bulk$measure == "usd", ]
  expect_equal(nrow(usd_rows), 2L)
  expect_true(all(usd_rows$series_id == "RBNZD.SUSD"))
  expect_setequal(unique(long_bulk$measure), c("usd", "aud"))
})
