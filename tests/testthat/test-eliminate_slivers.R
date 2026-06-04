## squares in a projected (metric) CRS; sliver "S" (area 5) shares a length-5
## border with "A" (25) and a length-1 border with "B" (10), so the LENGTH rule
## must absorb "S" into "A".
sq <- function(xmin, xmax, ymin, ymax, id, protected = FALSE, geom_col = "geometry") {
  p <- sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
  out <- sf::st_sf(id = id, protected = protected, geometry = sf::st_sfc(p, crs = 3005))
  if (geom_col != "geometry") {
    out <- sf::st_set_geometry(out, geom_col)
  }
  out
}

longest_border_fixture <- function(geom_col = "geometry") {
  rbind(
    sq(0, 5, 0, 5, "A", geom_col = geom_col),
    sq(0, 1, 6, 16, "B", geom_col = geom_col),
    sq(0, 5, 5, 6, "S", geom_col = geom_col)
  )
}

sf_area <- function(x, which_id) as.numeric(sf::st_area(x[x$id == which_id, ]))

test_that("a sliver is merged into its longest-shared-border neighbour", {
  res <- eliminate_slivers(longest_border_fixture(), threshold = 6)

  expect_s3_class(res, "sf")
  expect_setequal(res$id, c("A", "B"))
  expect_equal(sf_area(res, "A"), 30)
  expect_equal(sf_area(res, "B"), 10)
})

test_that("return type matches input type (sf -> sf, SpatVector -> SpatVector)", {
  fx <- longest_border_fixture()

  expect_s3_class(eliminate_slivers(fx, threshold = 6), "sf")

  res_v <- eliminate_slivers(terra::vect(fx), threshold = 6)
  expect_s4_class(res_v, "SpatVector")
  expect_setequal(res_v$id, c("A", "B"))
})

test_that("a units threshold is equivalent to the numeric (m^2) threshold", {
  fx <- longest_border_fixture()
  res_num <- eliminate_slivers(fx, threshold = 6)
  res_units <- eliminate_slivers(fx, threshold = units::set_units(6, "m^2"))

  expect_setequal(res_units$id, res_num$id)
  expect_equal(sf_area(res_units, "A"), sf_area(res_num, "A"))
})

test_that("`keep` protects a sub-threshold feature via a data-masking predicate", {
  fx <- rbind(sq(0, 10, 0, 10, "A"), sq(10, 12, 0, 4, "B", protected = TRUE))

  expect_setequal(eliminate_slivers(fx, threshold = 10)$id, "A")
  expect_setequal(eliminate_slivers(fx, threshold = 10, keep = protected)$id, c("A", "B"))
  expect_setequal(eliminate_slivers(fx, threshold = 10, keep = id == "B")$id, c("A", "B"))
})

test_that("no slivers returns the input unchanged", {
  fx <- longest_border_fixture()
  expect_identical(eliminate_slivers(fx, threshold = 1), fx)
})

test_that("the caller's geometry-column name is preserved", {
  res <- eliminate_slivers(longest_border_fixture(geom_col = "geom"), threshold = 6)
  expect_identical(attr(res, "sf_column"), "geom")
})

test_that("desliver is an alias for eliminate_slivers", {
  expect_identical(desliver, eliminate_slivers)
})

test_that("all-slivers warns and returns the input unchanged (pipe-safe)", {
  fx <- longest_border_fixture()
  expect_snapshot(res <- eliminate_slivers(fx, threshold = 1e6))
  expect_identical(res, fx)
})

test_that("a geographic CRS warns", {
  fx <- suppressWarnings(sf::st_set_crs(longest_border_fixture(), 4326))
  expect_snapshot(out <- eliminate_slivers(fx, threshold = 6))
})

test_that("non-polygon input is rejected", {
  pts <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3005)
  )
  expect_snapshot(eliminate_slivers(pts, threshold = 1), error = TRUE)
})
