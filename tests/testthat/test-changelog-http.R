library(testthat)

# HTTP error-path coverage for changelog helpers (.eolas_changes_get and friends).
# Mirrors the typed exceptions in eolas_data/client.py::_raw_changes_get.

test_that(".eolas_changes_get aborts with upgrade class on HTTP 402", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"Pro plan required"}', status = 402L)
    },
    .package = "eolas"
  )
  expect_error(
    eolas:::.eolas_changes_get("nz_parcels", 0L, 1L),
    class = "eolas_changes_upgrade_required"
  )
})


test_that(".eolas_changes_get aborts with licence class on HTTP 403 (licence detail)", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"licence: OECD prohibits export"}', status = 403L)
    },
    .package = "eolas"
  )
  expect_error(
    eolas:::.eolas_changes_get("oecd_gdp", 0L, 1L),
    class = "eolas_changes_licence_restricted"
  )
})


test_that(".eolas_changes_get aborts with auth class on HTTP 403 (inactive key)", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"API key is inactive"}', status = 403L)
    },
    .package = "eolas"
  )
  expect_error(
    eolas:::.eolas_changes_get("nz_cpi", 0L, 1L),
    class = "eolas_auth_error"
  )
})


test_that(".eolas_changes_get aborts with watermark-expired class on HTTP 410", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp(
        '{"error":"watermark_expired","min_available_seq":500}',
        status = 410L
      )
    },
    .package = "eolas"
  )
  expect_error(
    eolas:::.eolas_changes_get("nz_parcels", 5L, 100L),
    class = "eolas_watermark_expired"
  )
})


test_that(".eolas_fetch_seq_high returns 0 when changes GET fails", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp('{"detail":"Pro plan required"}', status = 402L)
    },
    .package = "eolas"
  )
  expect_equal(eolas:::.eolas_fetch_seq_high("nz_parcels"), 0)
})


test_that(".eolas_fetch_all_change_pages propagates watermark-expired condition", {
  set_test_key()
  local_mocked_bindings(
    eolas_http_perform = function(req) {
      httr2_mock_resp(
        '{"error":"watermark_expired","min_available_seq":42}',
        status = 410L
      )
    },
    .package = "eolas"
  )
  expect_error(
    eolas:::.eolas_fetch_all_change_pages("nz_parcels", since_seq = 5L),
    class = "eolas_watermark_expired"
  )
})