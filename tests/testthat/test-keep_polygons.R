rect <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
}

sf_of <- function(...) sf::st_sf(id = seq_len(...length()), geometry = sf::st_sfc(..., crs = 3005))

test_that("polygons pass through untouched", {
  x <- sf_of(rect(0, 10, 0, 10), rect(20, 30, 0, 10))

  expect_identical(keep_polygons(x), x)
})

test_that("a bare LINESTRING is dropped, with its attribute row", {
  ## Two polygons sharing only an edge intersect in a LINESTRING, with no GEOMETRYCOLLECTION
  ## anywhere in the result, so a guard testing only for collections never fires.
  x <- sf::st_sf(
    id = c("a", "b"),
    geometry = sf::st_sfc(rect(0, 10, 0, 10), rect(20, 30, 0, 10), crs = 3005)
  )
  y <- sf::st_sf(v = "y", geometry = sf::st_sfc(rect(10, 20, 0, 10), crs = 3005))
  i <- suppressWarnings(sf::st_intersection(x, y))

  expect_setequal(as.character(sf::st_geometry_type(i)), "LINESTRING")
  expect_equal(nrow(keep_polygons(i)), 0L)
  expect_named(keep_polygons(i), names(i))
})

test_that("polygons are recovered from a GEOMETRYCOLLECTION", {
  ## a square touching another square at a corner and overlapping a third: area plus a stray point
  gc <- sf::st_geometrycollection(list(rect(0, 10, 0, 10), sf::st_point(c(50, 50))))
  x <- sf::st_sf(id = "a", geometry = sf::st_sfc(gc, crs = 3005))

  out <- keep_polygons(x)

  expect_setequal(as.character(sf::st_geometry_type(out)), "POLYGON")
  expect_equal(sum(as.numeric(sf::st_area(out))), 100, tolerance = 1e-9)
})

test_that("empty geometries are dropped", {
  x <- sf::st_sf(
    id = c("a", "b"),
    geometry = sf::st_sfc(rect(0, 10, 0, 10), sf::st_polygon(), crs = 3005)
  )

  out <- keep_polygons(x)

  expect_equal(nrow(out), 1L)
  expect_identical(out$id, "a")
})

test_that("a mix of polygons and lines keeps only the polygons", {
  x <- sf::st_sf(
    id = c("poly", "line"),
    geometry = sf::st_sfc(
      rect(0, 10, 0, 10),
      sf::st_linestring(rbind(c(0, 0), c(10, 10))),
      crs = 3005
    )
  )

  out <- keep_polygons(x)

  expect_equal(nrow(out), 1L)
  expect_identical(out$id, "poly")
})
