sq <- function(xmin, xmax, ymin, ymax, crs = "EPSG:3857") {
  v <- terra::as.polygons(terra::ext(xmin, xmax, ymin, ymax))
  terra::crs(v) <- crs
  v
}
area <- function(v) sum(spatialutils::expanse_planar(v, "m"))

test_that("sym_difference matches the closed-form answer", {
  ## |A| = 4, |B| = 4, |A n B| = 1, so the symmetric difference is 4 + 4 - 2*1 = 6
  x <- sq(0, 2, 0, 2)
  y <- sq(1, 3, 1, 3)

  out <- sym_difference(x, y)

  expect_equal(area(out), 6)
  expect_setequal(out$source, c("x", "y"))
})

test_that("sym_difference agrees with terra::symdif on area", {
  x <- sq(0, 2, 0, 2)
  y <- sq(1, 3, 1, 3)

  expect_equal(area(sym_difference(x, y)), area(terra::symdif(x, y)))
})

test_that("sym_difference returns geometry only, plus a source tag", {
  x <- sq(0, 2, 0, 2)
  x$keep_me <- "a"
  y <- sq(1, 3, 1, 3)
  y$different_column <- "b"

  ## the two sides have different columns; rbind would NA-fill them against each other silently
  expect_identical(names(sym_difference(x, y)), "source")
})

test_that("sym_difference handles an empty side", {
  x <- sq(0, 2, 0, 2)
  empty <- x[integer(0), ]

  expect_equal(area(sym_difference(x, empty)), area(x))
  expect_equal(area(sym_difference(empty, x)), area(x))
})

test_that("sym_difference_tiled preserves area and splits at seams", {
  x <- sq(0, 2, 0, 2)
  y <- sq(1, 3, 1, 3)
  tiles <- rbind(sq(0, 1.5, 0, 3), sq(1.5, 3, 0, 3))

  untiled <- sym_difference(x, y)
  tiled <- sym_difference_tiled(x, y, tiles)

  ## area is the invariant; feature count is NOT -- the seam splits geometries
  expect_equal(area(tiled), area(untiled))
  expect_gt(nrow(tiled), nrow(untiled))
})

test_that("erase_polygons_tiled equals erase_polygons on area", {
  x <- sq(0, 4, 0, 4)
  y <- sq(1, 2, 1, 2)
  tiles <- rbind(sq(0, 2, 0, 4), sq(2, 4, 0, 4))

  expect_equal(area(erase_polygons_tiled(x, y, tiles)), area(erase_polygons(x, y)))
})

test_that("erase_polygons_tiled returns x unchanged when y is empty", {
  x <- sq(0, 4, 0, 4)
  tiles <- rbind(sq(0, 2, 0, 4), sq(2, 4, 0, 4))

  expect_equal(area(erase_polygons_tiled(x, x[integer(0), ], tiles)), area(x))
})

test_that("sf input returns sf, SpatVector input returns SpatVector", {
  x <- sq(0, 2, 0, 2)
  y <- sq(1, 3, 1, 3)
  sx <- sf::st_as_sf(x)
  sy <- sf::st_as_sf(y)

  expect_s3_class(erase_polygons(sx, sy), "sf")
  expect_s4_class(erase_polygons(x, y), "SpatVector")
  expect_s3_class(sym_difference(sx, sy), "sf")
  expect_s4_class(sym_difference(x, y), "SpatVector")
})

test_that("the sf path gives the same answer as the SpatVector path", {
  x <- sq(0, 2, 0, 2)
  y <- sq(1, 3, 1, 3)
  sx <- sf::st_as_sf(x)
  sy <- sf::st_as_sf(y)

  spat <- area(sym_difference(x, y))
  plain <- sum(as.numeric(sf::st_area(sym_difference(sx, sy))))

  expect_equal(plain, spat)
  expect_equal(plain, 6)
})

test_that("the tiled variants keep the class of x", {
  x <- sq(0, 4, 0, 4)
  y <- sq(1, 2, 1, 2)
  tiles <- rbind(sq(0, 2, 0, 4), sq(2, 4, 0, 4))

  expect_s4_class(erase_polygons_tiled(x, y, tiles), "SpatVector")
  expect_s3_class(erase_polygons_tiled(sf::st_as_sf(x), sf::st_as_sf(y), sf::st_as_sf(tiles)), "sf")
})

test_that("the sf path does not round-trip through terra", {
  ## the point of separate methods: an sf caller should never be pushed through SpatVector, which
  ## is the conversion `erase_polygons()` goes through sf to avoid in the first place
  x <- sf::st_as_sf(sq(0, 2, 0, 2))
  y <- sf::st_as_sf(sq(1, 3, 1, 3))

  called_vect <- FALSE
  testthat::local_mocked_bindings(
    vect = function(...) {
      called_vect <<- TRUE
      terra::vect(...)
    },
    .package = "terra"
  )

  invisible(erase_polygons(x, y))
  expect_false(called_vect)
})
