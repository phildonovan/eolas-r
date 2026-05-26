library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

# Fake Parquet bytes (PAR1 magic + minimal footer) — parseable by arrow.
# We use arrow::write_parquet() for tests that need real row counts.
.make_fake_parquet <- function(rows = 5L) {
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf))
  df <- data.frame(x = seq_len(rows), y = rep("a", rows))
  arrow::write_parquet(df, tf)
  readBin(tf, raw(), file.info(tf)$size)
}

FAKE_PARQUET_BYTES <- .make_fake_parquet(rows = 10L)

# Dataset metadata returned by GET /v1/datasets/{name}
.meta_body <- function(name = "doc_huts",
                       namespace = "doc",
                       snap_id = 1001,
                       incremental = TRUE) {
  jsonlite::toJSON(
    list(
      name                  = name,
      title                 = "DOC Huts",
      source                = "DOC",
      namespace             = namespace,
      table                 = name,
      current_snapshot_id   = snap_id,
      incremental_supported = incremental,
      refresh_cadence       = "monthly",
      geometry_type         = NULL,
      geometry_wkt          = NULL,
      has_geometry          = FALSE,
      row_count_at_last_refresh = 10L
    ),
    auto_unbox = TRUE, null = "null"
  )
}

# Build a fake httr2 response for the test mocks.
.fake_resp <- function(body, status = 200L, content_type = "application/json",
                       headers = list()) {
  all_headers <- c(
    list(`content-type` = content_type),
    headers
  )
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = as.integer(status),
      headers     = structure(all_headers, class = "httr2_headers"),
      body        = if (is.character(body)) charToRaw(body) else body,
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# ---------------------------------------------------------------------------
# Stateful mock helper
#
# We mock BOTH eolas_http_perform (for the incremental/bulk HTTP calls) AND
# eolas_info (for the metadata step).  Mocking eolas_info directly avoids
# vulnerability to stale namespace mocks from other test files
# (test-get-local.R mocks eolas_info via local_mocked_bindings(.env=ns) which
# does not always clean up reliably across test-file boundaries).
# ---------------------------------------------------------------------------
with_mock_sync <- function(meta_body,
                           data_responses,   # list of .fake_resp() objects
                           code) {
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  # Parse meta_body once so we can inject it via the eolas_info mock.
  parsed_meta <- jsonlite::fromJSON(meta_body, simplifyVector = FALSE)

  # data_responses counter: call 1 = eolas_http_perform (incremental), etc.
  data_call_count <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    # Mock eolas_info directly — bypasses eolas_http_perform for metadata.
    eolas_info = function(name, base_url = NULL) {
      parsed_meta
    },
    # Mock eolas_http_perform for incremental + bulk calls (after meta).
    eolas_http_perform = function(req) {
      data_call_count <<- data_call_count + 1L
      idx <- data_call_count
      if (idx > length(data_responses)) {
        .fake_resp(meta_body)  # safe fallback
      } else {
        data_responses[[idx]]
      }
    },
    .env = ns
  )
  code
}


# ===========================================================================
# sync_manifest helpers
# ===========================================================================

test_that(".eolas_write_manifest writes a valid JSON file atomically", {
  td <- withr::local_tempdir()
  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-27.parquet",
      synced_at   = "2026-05-27T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)
  manifest_path <- file.path(td, "doc_huts", "_eolas-manifest.json")
  expect_true(file.exists(manifest_path))
  parsed <- jsonlite::fromJSON(readLines(manifest_path, warn = FALSE),
                               simplifyVector = FALSE)
  expect_equal(parsed$dataset, "doc_huts")
  expect_equal(as.integer(parsed$current_snapshot), 1001L)
})

test_that(".eolas_read_manifest returns NULL when manifest absent", {
  td <- withr::local_tempdir()
  result <- .eolas_read_manifest(td, "nonexistent")
  expect_null(result)
})

test_that(".eolas_read_manifest parses an existing manifest", {
  td <- withr::local_tempdir()
  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-27.parquet",
      synced_at   = "2026-05-27T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)
  result <- .eolas_read_manifest(td, "doc_huts")
  expect_false(is.null(result))
  expect_equal(result$dataset, "doc_huts")
  expect_equal(as.numeric(result$current_snapshot), 1001)
})

test_that(".eolas_validate_manifest rejects wrong schema_version", {
  m <- list(
    dataset          = "doc_huts",
    snapshots        = list(),
    current_snapshot = NULL,
    format           = "parquet",
    schema_version   = 99L
  )
  expect_error(.eolas_validate_manifest(m), "schema_version")
})

test_that(".eolas_validate_manifest rejects invalid format", {
  m <- list(
    dataset          = "doc_huts",
    snapshots        = list(),
    current_snapshot = NULL,
    format           = "csv",
    schema_version   = 1L
  )
  expect_error(.eolas_validate_manifest(m), "format")
})

test_that(".eolas_validate_manifest rejects entry with bad file name", {
  m <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "bad_name.parquet",
      synced_at   = "2026-05-27T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  expect_error(.eolas_validate_manifest(m), "naming pattern")
})

test_that(".eolas_validate_manifest rejects entry with bad synced_at", {
  m <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-27.parquet",
      synced_at   = "2026-05-27",  # missing T and Z
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  expect_error(.eolas_validate_manifest(m), "ISO-8601")
})

test_that("atomic write leaves no tmp file on success", {
  td <- withr::local_tempdir()
  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(),
    current_snapshot = NULL,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)
  tmp_files <- list.files(file.path(td, "doc_huts"), pattern = "\\.tmp-")
  expect_equal(length(tmp_files), 0L)
})


# ===========================================================================
# eolas_sync() — decision logic branches
# ===========================================================================

test_that("eolas_sync: first sync returns status='snapshot_full'", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  with_mock_sync(
    meta_body       = .meta_body(snap_id = 1001L),
    data_responses  = list(
      .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                 content_type = "application/octet-stream")
    ),
    code = {
      r <- eolas_sync("doc_huts", library_dir = td)
      expect_equal(r$status, "snapshot_full")
      expect_equal(r$dataset, "doc_huts")
      expect_gt(r$bytes_downloaded, 0L)
      expect_equal(r$files_added, 1L)
      # Manifest written
      expect_true(file.exists(file.path(td, "doc_huts", "_eolas-manifest.json")))
    }
  )
})

test_that("eolas_sync: second sync same snapshot returns 'unchanged'", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  # Write a manifest with snapshot 1001 already present.
  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-27.parquet",
      synced_at   = "2026-05-27T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  with_mock_sync(
    meta_body      = .meta_body(snap_id = 1001L),
    data_responses = list(),
    code = {
      r <- eolas_sync("doc_huts", library_dir = td)
      expect_equal(r$status, "unchanged")
      expect_equal(r$bytes_downloaded, 0L)
      expect_equal(r$rows_added, 0L)
      expect_equal(r$files_added, 0L)
    }
  )
})

test_that("eolas_sync: new snapshot with incremental=TRUE returns 'snapshot_delta'", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-01.parquet",
      synced_at   = "2026-05-01T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  delta_bytes <- .make_fake_parquet(rows = 3L)

  with_mock_sync(
    meta_body      = .meta_body(snap_id = 1002L, incremental = TRUE),
    data_responses = list(
      # incremental endpoint returns delta
      .fake_resp(delta_bytes, status = 200L,
                 content_type = "application/octet-stream",
                 headers = list("X-Eolas-Row-Count" = "3",
                                "X-Eolas-Current-Snapshot" = "1002"))
    ),
    code = {
      r <- eolas_sync("doc_huts", library_dir = td)
      expect_equal(r$status, "snapshot_delta")
      expect_equal(r$rows_added, 3L)
      expect_equal(r$files_added, 1L)
      expect_gt(r$bytes_downloaded, 0L)

      # Updated manifest has delta entry
      m2 <- .eolas_read_manifest(td, "doc_huts")
      expect_equal(length(m2$snapshots), 2L)
      expect_equal(m2$snapshots[[2L]]$kind, "delta")
      expect_equal(as.numeric(m2$current_snapshot), 1002)
    }
  )
})

test_that("eolas_sync: 410 from incremental falls back to full download", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 999,
      kind        = "snapshot",
      file        = "snapshot-2026-01-01.parquet",
      synced_at   = "2026-01-01T10:00:00Z",
      rows        = 5L
    )),
    current_snapshot = 999,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  meta_410 <- jsonlite::fromJSON(
    .meta_body(snap_id = 2001L, incremental = TRUE), simplifyVector = FALSE
  )
  # Responses: first call = incremental (410), second call = bulk fallback
  call_count_410 <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) meta_410,
    eolas_http_perform   = function(req) {
      call_count_410 <<- call_count_410 + 1L
      if (call_count_410 == 1L) {
        # incremental endpoint → 410 Gone
        .fake_resp('{"detail":"Snapshot expired"}', status = 410L)
      } else {
        # bulk fallback
        .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                   content_type = "application/octet-stream")
      }
    },
    .env = ns
  )

  r <- eolas_sync("doc_huts", library_dir = td)
  expect_equal(r$status, "snapshot_full")
  expect_gt(r$bytes_downloaded, 0L)
})

test_that("eolas_sync: 400 from incremental falls back to full download", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 999,
      kind        = "snapshot",
      file        = "snapshot-2026-01-01.parquet",
      synced_at   = "2026-01-01T10:00:00Z",
      rows        = 5L
    )),
    current_snapshot = 999,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  meta_400 <- jsonlite::fromJSON(
    .meta_body(snap_id = 2001L, incremental = TRUE), simplifyVector = FALSE
  )
  call_count_400 <- 0L
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) meta_400,
    eolas_http_perform   = function(req) {
      call_count_400 <<- call_count_400 + 1L
      if (call_count_400 == 1L) {
        .fake_resp('{"detail":"incremental_supported=false"}', status = 400L)
      } else {
        .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                   content_type = "application/octet-stream")
      }
    },
    .env = ns
  )

  r <- eolas_sync("doc_huts", library_dir = td)
  expect_equal(r$status, "snapshot_full")
})

test_that("eolas_sync: incremental_supported=FALSE skips delta attempt", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-01.parquet",
      synced_at   = "2026-05-01T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  incremental_called <- FALSE
  parsed_meta <- jsonlite::fromJSON(.meta_body(snap_id = 2002L, incremental = FALSE),
                                    simplifyVector = FALSE)
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) parsed_meta,
    eolas_http_perform   = function(req) {
      if (grepl("incremental", req$url %||% "")) {
        incremental_called <<- TRUE
      }
      .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                 content_type = "application/octet-stream")
    },
    .env = ns
  )

  r <- eolas_sync("doc_huts", library_dir = td)
  expect_equal(r$status, "snapshot_full")
  expect_false(incremental_called)
})

test_that("eolas_sync: row_count=0 response returns 'unchanged'", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  manifest <- list(
    dataset          = "doc_huts",
    snapshots        = list(list(
      snapshot_id = 1001,
      kind        = "snapshot",
      file        = "snapshot-2026-05-01.parquet",
      synced_at   = "2026-05-01T10:00:00Z",
      rows        = 10L
    )),
    current_snapshot = 1001,
    format           = "parquet",
    schema_version   = 1L
  )
  .eolas_write_manifest(td, "doc_huts", manifest)

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  parsed_meta <- jsonlite::fromJSON(.meta_body(snap_id = 1002L, incremental = TRUE),
                                    simplifyVector = FALSE)
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) parsed_meta,
    eolas_http_perform   = function(req) {
      # 200 with X-Eolas-Row-Count: 0
      .fake_resp("", status = 200L,
                 content_type = "application/octet-stream",
                 headers = list("X-Eolas-Row-Count" = "0"))
    },
    .env = ns
  )

  r <- eolas_sync("doc_huts", library_dir = td)
  expect_equal(r$status, "unchanged")
  expect_equal(r$rows_added, 0L)
})


# ===========================================================================
# eolas_sync_all()
# ===========================================================================

test_that("eolas_sync_all: all-success returns named list of sync results", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  meta_all <- jsonlite::fromJSON(
    .meta_body(snap_id = 5001L, incremental = FALSE), simplifyVector = FALSE
  )
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) meta_all,
    eolas_http_perform   = function(req) {
      .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                 content_type = "application/octet-stream")
    },
    .env = ns
  )

  results <- eolas_sync_all(
    library_dir    = td,
    datasets       = c("doc_huts", "nz_cpi"),
    max_concurrent = 1L
  )
  expect_equal(length(results), 2L)
  expect_true(all(vapply(results, function(r) r$status == "snapshot_full",
                         logical(1L))))
  expect_named(results, c("doc_huts", "nz_cpi"))
})

test_that("eolas_sync_all: per-dataset error does not abort others", {
  skip_if_not_installed("arrow")
  td <- withr::local_tempdir()
  withr::local_envvar(EOLAS_NO_PROGRESS = "1")

  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  meta_nz_cpi <- jsonlite::fromJSON(
    .meta_body(name = "nz_cpi", namespace = "statsnz",
               snap_id = 9001L, incremental = FALSE),
    simplifyVector = FALSE
  )
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    # doc_huts → error; nz_cpi → success
    eolas_info = function(name, base_url = NULL) {
      if (identical(name, "doc_huts")) {
        stop("Not found: doc_huts")
      }
      meta_nz_cpi
    },
    eolas_http_perform = function(req) {
      .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                 content_type = "application/octet-stream")
    },
    .env = ns
  )

  results <- eolas_sync_all(
    library_dir    = td,
    datasets       = c("doc_huts", "nz_cpi"),
    max_concurrent = 1L
  )
  expect_equal(length(results), 2L)
  statuses <- vapply(results, `[[`, character(1L), "status")
  expect_true("error" %in% statuses)
  expect_true("snapshot_full" %in% statuses)
})

test_that("eolas_sync_all: auto-discover discovers datasets with manifests", {
  td <- withr::local_tempdir()

  # Create two fake synced datasets (manifests only, no real files needed for
  # discovery).
  for (nm in c("a_dataset", "b_dataset")) {
    m <- list(
      dataset          = nm,
      snapshots        = list(),
      current_snapshot = NULL,
      format           = "parquet",
      schema_version   = 1L
    )
    .eolas_write_manifest(td, nm, m)
  }

  # Mock: mark datasets as unchanged (snapshot id matches manifest which has
  # current_snapshot = NULL → treated as no local manifest... actually we need
  # a non-null current_snapshot for the no-op path).  Simplest: mock server
  # returns same snap id (0) as NULL casts to.
  ns <- getNamespace("eolas")
  assign("key", "eolas_testkey", envir = ns$.eolas_env)

  # Return snap_id = 0, and manifests have current_snapshot = NULL → NULL != 0,
  # but as.numeric(NULL) = numeric(0) → comparison `local_snap == 0` fails.
  # So both datasets will fall through to full download.
  meta_zero <- jsonlite::fromJSON(
    .meta_body(snap_id = 0L, incremental = FALSE), simplifyVector = FALSE
  )
  local_mocked_bindings(
    .eolas_use_streaming = function() FALSE,
    eolas_info = function(name, base_url = NULL) meta_zero,
    eolas_http_perform   = function(req) {
      .fake_resp(FAKE_PARQUET_BYTES, status = 200L,
                 content_type = "application/octet-stream")
    },
    .env = ns
  )

  withr::with_envvar(list(EOLAS_NO_PROGRESS = "1"), {
    results <- eolas_sync_all(library_dir = td, max_concurrent = 1L)
  })
  expect_equal(length(results), 2L)
  expect_true(all(names(results) %in% c("a_dataset", "b_dataset")))
})

test_that("eolas_sync_all: returns empty list when no manifests found", {
  td <- withr::local_tempdir()
  results <- eolas_sync_all(library_dir = td)
  expect_equal(length(results), 0L)
})


# ===========================================================================
# print methods
# ===========================================================================

test_that("print.eolas_sync_result works for snapshot_full", {
  r <- .new_sync_result("snapshot_full", "doc_huts", "/data",
                        bytes_downloaded = 1000L, rows_added = 10L,
                        files_added = 1L)
  out <- capture.output(print(r))
  expect_true(grepl("snapshot_full", out))
  expect_true(grepl("doc_huts", out))
})

test_that("print.eolas_sync_result works for error", {
  r <- .new_sync_result("error", "doc_huts", "/data",
                        bytes_downloaded = 0L, rows_added = 0L,
                        files_added = 0L, error = "timeout")
  out <- capture.output(print(r))
  expect_true(grepl("error", out))
  expect_true(grepl("timeout", out))
})
