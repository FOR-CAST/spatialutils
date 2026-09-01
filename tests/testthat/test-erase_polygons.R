rect <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
}

vect_of <- function(nm, values, ...) {
  terra::vect(sf::st_sf(stats::setNames(list(values), nm), geometry = sf::st_sfc(..., crs = 3005)))
}

area_m2 <- function(v) {
  if (nrow(v) == 0L) 0 else sum(terra::expanse(v, unit = "m", transform = FALSE))
}

synced <- function(v) nrow(v) == nrow(terra::values(v))

test_that("the difference keeps x's attributes and loses exactly y's area", {
  x <- vect_of("id", c("a", "b"), rect(0, 100, 0, 100), rect(100, 200, 0, 100))
  y <- vect_of("v", "y", rect(50, 150, 0, 100))

  out <- erase_polygons(x, y)

  expect_true(synced(out))
  expect_equal(area_m2(out), 2 * 100 * 100 - 100 * 100, tolerance = 1e-9)
  expect_setequal(out$id, c("a", "b"))
  expect_named(out, "id")
})

test_that("an empty y returns x unchanged", {
  ## `sf::st_union()` of an empty layer is a zero-length geometry, and `st_difference()` against
  ## that fails inside sf with "replacement has 1 row, data has 0" -- which says nothing about the
  ## cause, and which every tile with nothing to erase would hit.
  x <- vect_of("id", "a", rect(0, 100, 0, 100))
  y <- vect_of("v", "y", rect(500, 600, 500, 600))

  expect_equal(area_m2(erase_polygons(x, y[integer(0), ])), area_m2(x), tolerance = 1e-9)
  expect_named(erase_polygons(x, y[integer(0), ]), "id")
})

test_that("y covering x completely gives an empty result that keeps the columns", {
  x <- vect_of("id", "a", rect(0, 100, 0, 100))
  y <- vect_of("v", "y", rect(-10, 110, -10, 110))

  out <- erase_polygons(x, y)

  expect_equal(nrow(out), 0L)
  expect_named(out, "id")
  expect_true(synced(out))
})

test_that("a difference reduced to a line does not desynchronise the result", {
  ## Everything left of the polygon is a zero-width spike, so `st_difference()` returns a bare
  ## LINESTRING -- not a GEOMETRYCOLLECTION. `terra::vect()` drops that geometry while keeping its
  ## attribute row, and the SpatVector then fails somewhere else entirely.
  spike <- sf::st_polygon(list(cbind(c(0, 10, 10, 15, 10, 10, 0, 0), c(0, 0, 5, 5, 5, 10, 10, 0))))
  x <- vect_of("id", "a", spike)
  y <- vect_of("v", "y", rect(-1, 10, -1, 11))

  out <- erase_polygons(x, y)

  expect_true(synced(out))
  expect_equal(nrow(out), 0L)
})
