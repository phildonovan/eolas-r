test_that(".eolas_sanitize_detail collapses a Cloudflare HTML error page", {
  resp <- httr2::response(status_code = 502, headers = list("cf-ray" = "8abc123-AKL"))
  html <- paste0(
    "<!DOCTYPE html><html><head><title>Cloudflare</title></head>",
    "<body>", paste(rep("noise ", 500), collapse = ""), "</body></html>"
  )
  out <- .eolas_sanitize_detail(html, resp)
  expect_false(grepl("<html", out, ignore.case = TRUE))
  expect_match(out, "cf-ray 8abc123-AKL")
  expect_lt(nchar(out), 200)
})

test_that(".eolas_sanitize_detail truncates a runaway non-HTML detail", {
  resp <- httr2::response(status_code = 500)
  out <- .eolas_sanitize_detail(strrep("x", 2000), resp)
  expect_lt(nchar(out), 520)
  expect_match(out, "truncated")
})

test_that(".eolas_sanitize_detail passes a normal short detail through unchanged", {
  resp <- httr2::response(status_code = 400)
  expect_equal(.eolas_sanitize_detail("dataset not found", resp), "dataset not found")
})

test_that(".eolas_sanitize_detail handles a non-character detail", {
  resp <- httr2::response(status_code = 400)
  expect_equal(.eolas_sanitize_detail(NULL, resp), "Unknown error")
})

test_that("eolas_download_bulk rejects unknown arguments", {
  # `...` is reserved; a misspelled arg must error, not be silently ignored.
  expect_error(
    eolas_download_bulk("nz_cpi", dest_dir = "/tmp"),
    class = "rlang_error"
  )
})
