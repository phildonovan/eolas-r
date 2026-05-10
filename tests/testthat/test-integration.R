library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

INTEGRATION_BODY <- jsonlite::toJSON(
  list(
    platform = "meltano",
    files = list(
      "meltano.yml"   = "# generated meltano config\nname: tap-eolas\n",
      "README.md"     = "# eolas -> Meltano\n\nrun `meltano install`\n",
      ".env.example"  = "EOLAS_API_KEY=your_key\n"
    )
  ),
  auto_unbox = TRUE
)

FIVETRAN_BODY <- jsonlite::toJSON(
  list(
    platform = "fivetran",
    files = list("fivetran.yml" = "name: eolas\n", "README.md" = "# fivetran\n")
  ),
  auto_unbox = TRUE
)

ENTERPRISE_403_BODY <- jsonlite::toJSON(
  list(detail = "This endpoint is an Enterprise plan feature. See https://eolas.fyi/pricing"),
  auto_unbox = TRUE
)

# ---------------------------------------------------------------------------
# Returns files in memory
# ---------------------------------------------------------------------------

test_that("eolas_integration returns the files map", {
  with_mock_eolas(INTEGRATION_BODY, code = {
    result <- eolas_integration("meltano", c("nz_cpi", "nz_gdp"))
    expect_equal(result$platform, "meltano")
    expect_named(result$files, c("meltano.yml", "README.md", ".env.example"))
    expect_match(result$files$meltano.yml, "tap-eolas", fixed = TRUE)
  })
})

test_that("eolas_integration works for fivetran too", {
  with_mock_eolas(FIVETRAN_BODY, code = {
    result <- eolas_integration("fivetran", "nz_cpi")
    expect_equal(result$platform, "fivetran")
    expect_named(result$files, c("fivetran.yml", "README.md"))
  })
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("eolas_integration rejects empty datasets", {
  expect_error(
    eolas_integration("meltano", character(0)),
    "non-empty character vector"
  )
})

test_that("eolas_integration rejects empty platform", {
  expect_error(
    eolas_integration("", "nz_cpi"),
    "non-empty string"
  )
})

# ---------------------------------------------------------------------------
# Enterprise gate flows through verbatim
# ---------------------------------------------------------------------------

test_that("non-Enterprise plan surfaces the server's upgrade message", {
  with_mock_eolas(ENTERPRISE_403_BODY, status = 403L, code = {
    expect_error(
      eolas_integration("meltano", "nz_cpi"),
      "Enterprise plan feature",
      fixed = TRUE
    )
  })
})

# ---------------------------------------------------------------------------
# Output dir: write to disk
# ---------------------------------------------------------------------------

test_that("eolas_integration writes files when output_dir is set", {
  tmp <- withr::local_tempdir()
  with_mock_eolas(INTEGRATION_BODY, code = {
    result <- eolas_integration("meltano", "nz_cpi", output_dir = tmp)
    expect_length(result$written, 3L)
    expect_length(result$skipped, 0L)
    expect_true(file.exists(file.path(tmp, "meltano.yml")))
    expect_true(file.exists(file.path(tmp, "README.md")))
    expect_true(file.exists(file.path(tmp, ".env.example")))
    expect_match(readLines(file.path(tmp, "meltano.yml"))[1], "generated meltano")
  })
})

test_that("eolas_integration preserves existing files without force", {
  tmp <- withr::local_tempdir()
  writeLines("DO NOT OVERWRITE", file.path(tmp, "meltano.yml"))
  with_mock_eolas(INTEGRATION_BODY, code = {
    result <- eolas_integration("meltano", "nz_cpi", output_dir = tmp)
    expect_length(result$skipped, 1L)
    expect_equal(readLines(file.path(tmp, "meltano.yml")), "DO NOT OVERWRITE")
    # Other files still got written
    expect_true(file.exists(file.path(tmp, "README.md")))
  })
})

test_that("eolas_integration force=TRUE overwrites", {
  tmp <- withr::local_tempdir()
  writeLines("OLD", file.path(tmp, "meltano.yml"))
  with_mock_eolas(INTEGRATION_BODY, code = {
    result <- eolas_integration("meltano", "nz_cpi",
                                output_dir = tmp, force = TRUE)
    expect_length(result$skipped, 0L)
    expect_match(readLines(file.path(tmp, "meltano.yml"))[1], "generated meltano")
  })
})

test_that("eolas_integration creates output_dir if missing", {
  tmp <- withr::local_tempdir()
  new_dir <- file.path(tmp, "fresh", "nested", "dir")
  with_mock_eolas(INTEGRATION_BODY, code = {
    eolas_integration("meltano", "nz_cpi", output_dir = new_dir)
    expect_true(dir.exists(new_dir))
    expect_true(file.exists(file.path(new_dir, "meltano.yml")))
  })
})
