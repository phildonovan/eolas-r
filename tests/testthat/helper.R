library(testthat)

# Shared HTTP mock helpers for the eolas package test suite.
# Always mock via with_mocked_bindings(..., .package = "eolas") so bindings
# restore cleanly between tests (local_mocked_bindings(.env = ns) leaked).

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

FAKE_PARQUET <- c(charToRaw("PAR1"), as.raw(rep(0L, 12)), charToRaw("PAR1"))
FAKE_PARQUET_V2 <- c(charToRaw("PAR1"), as.raw(rep(1L, 12)), charToRaw("PAR1"))
SNAPSHOT_V1 <- "5503437996448954328"
SNAPSHOT_V2 <- "7041234567890123456"
SNAPSHOT_ID <- "snap_abc123"

BULK_DATASET_META <- jsonlite::toJSON(
  list(
    name      = "nz_cpi",
    title     = "NZ Consumer Price Index",
    source    = "Stats NZ",
    namespace = "statsnz",
    table     = "nz_cpi"
  ),
  auto_unbox = TRUE
)

DATASET_META_NON_GEO <- jsonlite::toJSON(
  list(name = "nz_cpi", title = "NZ CPI", source = "Stats NZ",
       namespace = "statsnz", table = "nz_cpi"),
  auto_unbox = TRUE
)

DATASET_META_GEO <- jsonlite::toJSON(
  list(name = "nz_parcels", title = "NZ Parcels", source = "LINZ",
       namespace = "linz", table = "nz_parcels",
       geometry_type = "MultiPolygon",
       bulk_export_class = "geoparquet",
       row_count_at_last_refresh = 3000000L),
  auto_unbox = TRUE
)

# ---------------------------------------------------------------------------
# Key + httr2 response builders
# ---------------------------------------------------------------------------

set_test_key <- function() {
  ns <- asNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)
  invisible(NULL)
}

httr2_mock_resp <- function(body, status = 200L) {
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = status,
      headers     = structure(
        list(`content-type` = "application/json"),
        class = "httr2_headers"
      ),
      body  = charToRaw(body),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

httr2_mock_resp_raw <- function(body, status = 200L,
                                content_type = "application/octet-stream",
                                extra_headers = list()) {
  headers_list <- c(
    list(
      `content-type`   = content_type,
      `content-length` = as.character(length(body))
    ),
    extra_headers
  )
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = status,
      headers     = structure(headers_list, class = "httr2_headers"),
      body        = body,
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

httr2_mock_head_resp <- function(snapshot_id, status = 200L) {
  structure(
    list(
      method      = "HEAD",
      url         = "https://api.eolas.fyi/test",
      status_code = status,
      headers     = structure(
        list(
          `content-type`       = "application/json",
          `X-Snapshot-Version` = snapshot_id
        ),
        class = "httr2_headers"
      ),
      body  = raw(0L),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

httr2_mock_resp_empty <- function(status) {
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = as.integer(status),
      headers     = structure(list(`content-type` = ""), class = "httr2_headers"),
      body        = raw(0L),
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

fake_sync_result <- function(path, snapshot_id = SNAPSHOT_ID) {
  list(
    status               = "downloaded",
    previous_snapshot_id = NA_character_,
    current_snapshot_id  = snapshot_id,
    path                 = normalizePath(path, mustWork = FALSE),
    bytes_downloaded     = 1024L
  )
}

write_test_sidecar <- function(data_path, snapshot_id) {
  sidecar_path <- paste0(data_path, ".eolas-meta.json")
  data <- list(
    schema_version = 1L,
    name           = "nz_cpi",
    snapshot_id    = snapshot_id,
    format         = "parquet",
    freshness      = "auto",
    downloaded_at  = "2026-05-24T01:23:45Z",
    source_url     = "https://api.eolas.fyi/v1/bulk/statsnz/nz_cpi?format=parquet"
  )
  writeLines(jsonlite::toJSON(data, auto_unbox = TRUE), sidecar_path)
}

# ---------------------------------------------------------------------------
# Generic API mock (single JSON response for every HTTP call)
# ---------------------------------------------------------------------------

MOCK_DATASET_INFO <- '{"name":"nz_cpi","title":"Test dataset","source":"Stats NZ","namespace":"statsnz"}'

with_mock_eolas <- function(body, status = 200L, code) {
  set_test_key()
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        httr2_mock_resp(body, status)
      } else if (grepl("/v1/datasets/[^/]+($|\\?)", url)) {
        httr2_mock_resp(MOCK_DATASET_INFO, status)
      } else {
        httr2_mock_resp(body, status)
      }
    },
    .package = "eolas"
  )
}

# ---------------------------------------------------------------------------
# Bulk download: metadata GET then bulk GET
# ---------------------------------------------------------------------------

with_mock_bulk <- function(bulk_body,
                           bulk_status  = 200L,
                           bulk_content = "application/octet-stream",
                           meta_body    = BULK_DATASET_META,
                           meta_status  = 200L,
                           code) {
  set_test_key()
  call_count <- 0L
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        httr2_mock_resp(meta_body, meta_status)
      } else {
        body <- if (is.character(bulk_body)) charToRaw(bulk_body) else bulk_body
        httr2_mock_resp_raw(body, bulk_status, bulk_content)
      }
    },
    .package = "eolas"
  )
}

# ---------------------------------------------------------------------------
# sync_bulk: metadata GET, HEAD, optional bulk GET
# ---------------------------------------------------------------------------

with_mock_sync <- function(snapshot_id,
                           bulk_body    = FAKE_PARQUET,
                           bulk_status  = 200L,
                           bulk_content = "application/octet-stream",
                           meta_body    = BULK_DATASET_META,
                           meta_status  = 200L,
                           code) {
  set_test_key()
  call_count <- 0L
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        httr2_mock_resp(meta_body, meta_status)
      } else if (call_count == 2L) {
        httr2_mock_head_resp(snapshot_id)
      } else {
        body <- if (is.character(bulk_body)) charToRaw(bulk_body) else bulk_body
        httr2_mock_resp_raw(body, bulk_status, bulk_content)
      }
    },
    .package = "eolas"
  )
}

with_mock_sync_unchanged <- function(snapshot_id,
                                     meta_body = BULK_DATASET_META,
                                     code) {
  set_test_key()
  call_count <- 0L
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        httr2_mock_resp(meta_body, 200L)
      } else {
        httr2_mock_head_resp(snapshot_id)
      }
    },
    .package = "eolas"
  )
}

# ---------------------------------------------------------------------------
# get_local: stub info + sync_bulk (file_writer writes the cached file)
# ---------------------------------------------------------------------------

with_mock_get_local <- function(meta_json, file_writer, code) {
  set_test_key()
  with_mocked_bindings(
    code,
    eolas_info = function(n, base_url = NULL) {
      jsonlite::fromJSON(meta_json, simplifyVector = FALSE)
    },
    eolas_sync_bulk = function(n, path, format, freshness, base_url = NULL, ...) {
      file_writer(path)
      fake_sync_result(path)
    },
    .package = "eolas"
  )
}