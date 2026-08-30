library(testthat)

# C22 -- the Free-plan row cap must never be presented as the full dataset.
# The server marks a capped /data response with X-Eolas-Truncated: true and
# X-Plan-Row-Cap; the client must warn, stamp eolas_meta()$truncated, and say
# that limit = N on a capped slice is not "the most recent N".

TRUNC_DATA_BODY <- '{"data":[
  {"date":"2018-01-01","period":"2018Q1","value":1.0},
  {"date":"2020-01-01","period":"2020Q1","value":2.0},
  {"date":"2024-01-01","period":"2024Q1","value":3.0},
  {"date":"2025-01-01","period":"2025Q1","value":4.0}
]}'

CAP_HEADERS <- list(
  `X-Eolas-Truncated` = "true",
  `X-Plan-Row-Cap`    = "50000",
  `X-Plan`            = "free"
)

httr2_mock_resp_headers <- function(body, headers, status = 200L,
                                    content_type = "application/json") {
  structure(
    list(
      method      = "GET",
      url         = "https://api.eolas.fyi/test",
      status_code = status,
      headers     = structure(c(list(`content-type` = content_type), headers),
                              class = "httr2_headers"),
      body  = charToRaw(body),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

with_mock_capped <- function(data_headers, code, body = TRUNC_DATA_BODY) {
  set_test_key()
  with_mocked_bindings(
    code,
    .eolas_use_streaming = function() FALSE,
    eolas_http_perform = function(req) {
      url <- httr2::req_get_url(req)
      if (grepl("/data($|\\?)", url)) {
        httr2_mock_resp_headers(body, data_headers)
      } else {
        httr2_mock_resp(MOCK_DATASET_INFO)
      }
    },
    .package = "eolas"
  )
}

test_that(".eolas_truncation_from_headers parses the cap contract", {
  resp <- httr2_mock_resp_headers("{}", CAP_HEADERS)
  out <- .eolas_truncation_from_headers(resp)
  expect_true(out$truncated)
  expect_equal(out$row_cap, 50000L)
  expect_equal(out$plan, "free")

  expect_equal(.eolas_truncation_from_headers(NULL), list())
  expect_equal(.eolas_truncation_from_headers(httr2_mock_resp("{}")), list())
  off <- .eolas_truncation_from_headers(
    httr2_mock_resp_headers("{}", list(`X-Eolas-Truncated` = "false"))
  )
  expect_false(off$truncated)
})

test_that(".eolas_merge_truncation stamps meta even when meta_info is NULL", {
  m <- .eolas_merge_truncation(NULL, list(truncated = TRUE, row_cap = 50000L))
  expect_true(is.data.frame(m))
  expect_true(m$truncated)
  expect_equal(m$row_cap, 50000L)
  expect_null(.eolas_merge_truncation(NULL, list()))
})

test_that("eolas_get warns and stamps eolas_meta when the response is plan-capped", {
  with_mock_capped(CAP_HEADERS, code = {
    expect_warning(
      df <- eolas_get("nz_cpi"),
      "truncated to 50,000 rows"
    )
    expect_true(isTRUE(eolas_meta(df)$truncated))
    expect_equal(eolas_meta(df)$row_cap, 50000L)
    output <- c(
      capture.output(print(df)),
      capture.output(print(df), type = "message")
    )
    expect_true(any(grepl("TRUNCATED to 50,000 rows", output)))
  })
})

test_that("eolas_get limit= on a capped slice says it is not the most recent N", {
  with_mock_capped(CAP_HEADERS, code = {
    expect_warning(
      df <- eolas_get("nz_cpi", limit = 2L),
      "WITHIN that slice"
    )
    expect_equal(nrow(df), 2L)
    expect_true(isTRUE(eolas_meta(df)$truncated))
  })
})

test_that("eolas_get with meta = FALSE still surfaces truncation", {
  with_mock_capped(CAP_HEADERS, code = {
    expect_warning(df <- eolas_get("nz_cpi", meta = FALSE), "truncated")
    expect_true(isTRUE(eolas_meta(df)$truncated))
  })
})

test_that("eolas_get does not warn when X-Eolas-Truncated is false", {
  with_mock_capped(list(`X-Eolas-Truncated` = "false"), code = {
    expect_no_warning(df <- eolas_get("nz_cpi"))
    expect_false(isTRUE(eolas_meta(df)$truncated))
    expect_true("truncated" %in% names(eolas_meta(df)))
  })
})

test_that("eolas_get does not warn on an old server without the header", {
  with_mock_capped(list(), code = {
    expect_no_warning(df <- eolas_get("nz_cpi"))
    expect_false("truncated" %in% names(eolas_meta(df)))
  })
})

test_that("eolas_get as_arrow = TRUE still warns on a capped response", {
  skip_if_not_installed("arrow")
  with_mock_capped(CAP_HEADERS, code = {
    expect_warning(tbl <- eolas_get("nz_cpi", as_arrow = TRUE), "truncated")
    expect_true(inherits(tbl, "Table"))
  })
})
