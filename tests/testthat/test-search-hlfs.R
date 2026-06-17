library(testthat)

HLFS_LIST_BODY <- '[
  {"name":"leed_firms_employment","title":"LEED employment","source":"Stats NZ","namespace":"statsnz"},
  {"name":"nz_unemployment","title":"NZ Unemployment Rate","source":"OECD","namespace":"oecd"},
  {"name":"rbnz_m9_labour_market","title":"NZ Labour Market (RBNZ M9)","source":"RBNZ","namespace":"rbnz"}
]'

test_that("eolas_search HLFS is tight and ranked", {
  set_test_key()
  with_mocked_bindings({
    out <- eolas_search("HLFS")
    expect_false("leed_firms_employment" %in% out$name)
    expect_equal(out$name[1], "nz_unemployment")
    expect_equal(out$name[2], "rbnz_m9_labour_market")
  },
  .eolas_use_streaming = function() FALSE,
  eolas_http_perform = function(req) httr2_mock_resp(HLFS_LIST_BODY),
  .package = "eolas")
})