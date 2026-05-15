library(testthat)

# Regression: a single row with absent/blank/unparseable geometry must not
# abort the whole sf conversion (NZ meshblock data legitimately contains
# non-digitised "oceanic" / "area outside region" meshblocks). Before the
# fix, eolas_get_statsnz_geo("nz_meshblock_2023", as_sf = TRUE) died with
# "OGR: Unsupported geometry type".

GEO_BODY <- '{"data":[
  {"id":"A","geometry_wkt":"MULTIPOLYGON(((174 -36,175 -36,175 -37,174 -36)))"},
  {"id":"B","geometry_wkt":null},
  {"id":"C","geometry_wkt":""},
  {"id":"D","geometry_wkt":"None"},
  {"id":"E","geometry_wkt":"NOT_WKT_AT_ALL"},
  {"id":"F","geometry_wkt":"POINT(174 -36)"}
]}'

test_that("blank/null/garbage geometry rows don't abort sf conversion", {
  skip_if_not_installed("sf")

  expect_warning(
    res <- with_mock_eolas(GEO_BODY, code = {
      eolas_get_statsnz_geo("nz_meshblock_2023", as_sf = TRUE)
    }),
    "no usable geometry"
  )

  # Every attribute row is preserved (no silent drops)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 6L)
  expect_equal(res$id, c("A", "B", "C", "D", "E", "F"))

  # 4 rows have EMPTY geometry: B/C/D blank, E unparseable. A/F are real.
  empty <- sf::st_is_empty(res)
  expect_equal(sum(empty), 4L)
  expect_false(empty[res$id == "A"])
  expect_false(empty[res$id == "F"])
  expect_true(all(empty[res$id %in% c("B", "C", "D", "E")]))

  # Geometry column is "geometry", CRS is WGS84, raw WKT column dropped
  expect_equal(attr(res, "sf_column"), "geometry")
  expect_false("geometry_wkt" %in% names(res))
  expect_equal(sf::st_crs(res)$epsg, 4326L)
})

test_that("all-valid geometry still converts cleanly with no warning", {
  skip_if_not_installed("sf")

  body <- '{"data":[
    {"id":"A","geometry_wkt":"POINT(174 -36)"},
    {"id":"B","geometry_wkt":"POINT(175 -41)"}
  ]}'
  res <- with_mock_eolas(body, code = {
    eolas_get_statsnz_geo("x", as_sf = TRUE)
  })
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2L)
  expect_false(any(sf::st_is_empty(res)))
})
