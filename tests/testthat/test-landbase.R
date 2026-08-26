mk_square <- function(xmin, xmax, ymin, ymax, crs = "EPSG:3857") {
  v <- terra::as.polygons(terra::ext(xmin, xmax, ymin, ymax))
  terra::crs(v) <- crs
  v
}

test_that("read_vector_aoi keeps only features related to the AOI", {
  aoi <- mk_square(0, 5, 0, 5)
  near <- mk_square(1, 2, 1, 2)
  far <- mk_square(20, 21, 20, 21)
  src <- rbind(near, far)
  src$id <- c("near", "far")

  out <- read_vector_aoi(src, aoi)
  expect_s4_class(out, "SpatVector")
  expect_identical(out$id, "near")
})

test_that("read_vector_aoi subsets to requested fields", {
  aoi <- mk_square(0, 5, 0, 5)
  src <- mk_square(1, 2, 1, 2)
  src$keep <- "a"
  src$drop <- "b"

  out <- read_vector_aoi(src, aoi, fields = "keep")
  expect_identical(names(out), "keep")
})

test_that("read_vector_aoi pushes the spatial filter down to a file read", {
  aoi <- mk_square(-1, 7, -1, 7)
  near <- rbind(mk_square(0, 1, 0, 1), mk_square(5, 6, 5, 6))
  far <- mk_square(20, 21, 20, 21)
  src <- rbind(near, far)
  src$status <- c("A", "B", "A")

  f <- withr::local_tempfile(fileext = ".gpkg")
  terra::writeVector(src, f)

  out <- read_vector_aoi(f, aoi, fields = "status")
  expect_equal(nrow(out), 2)
  expect_identical(names(out), "status")
})

test_that("read_vector_aoi returns an empty SpatVector when nothing overlaps", {
  aoi <- mk_square(0, 1, 0, 1)
  src <- mk_square(20, 21, 20, 21)

  expect_equal(nrow(read_vector_aoi(src, aoi)), 0)
})

test_that("intersect_relate equals terra::intersect for overlapping inputs", {
  x <- mk_square(0, 3, 0, 3)
  y <- mk_square(2, 5, 2, 5)

  out <- intersect_relate(x, y)
  expect_equal(terra::expanse(out), terra::expanse(terra::intersect(x, y)))
})

test_that("intersect_relate returns empty for disjoint inputs", {
  x <- mk_square(0, 1, 0, 1)
  y <- mk_square(10, 11, 10, 11)

  expect_equal(nrow(intersect_relate(x, y)), 0)
})

test_that("prep_landbase (vector) dissolves by status within the AOI", {
  aoi <- mk_square(0, 4, 0, 4)
  a1 <- mk_square(0, 2, 0, 2)
  a2 <- mk_square(2, 4, 0, 2)
  p1 <- mk_square(0, 4, 2, 4)
  src <- rbind(a1, a2, p1)
  src$f_active <- c("Active", "Active", "Passive")

  out <- prep_landbase(src, aoi, status_col = "f_active", dissolve = "vector")
  expect_identical(sort(out$lbstatus), c("Active", "Passive"))
  expect_identical(names(out), "lbstatus")
  ## the two Active squares dissolve into a single feature
  expect_equal(nrow(out), 2)
})

test_that("prep_landbase (raster) dissolves by status", {
  aoi <- mk_square(0, 4, 0, 4)
  a1 <- mk_square(0, 2, 0, 2)
  a2 <- mk_square(2, 4, 0, 2)
  p1 <- mk_square(0, 4, 2, 4)
  src <- rbind(a1, a2, p1)
  src$f_active <- c("Active", "Active", "Passive")

  out <- prep_landbase(src, aoi, status_col = "f_active", dissolve = "raster", dissolve_res = 0.5)
  expect_setequal(out$lbstatus, c("Active", "Passive"))
  expect_identical(names(out), "lbstatus")
})

test_that("prep_landbase clips to the AOI when mask = TRUE", {
  aoi <- mk_square(0, 2, 0, 2)
  src <- mk_square(0, 4, 0, 4)
  src$status <- "Active"

  masked <- prep_landbase(
    src,
    aoi,
    status_col = "status",
    status_out = "status",
    dissolve = "vector"
  )
  cropped <- prep_landbase(
    src,
    aoi,
    status_col = "status",
    status_out = "status",
    dissolve = "vector",
    mask = FALSE
  )
  expect_equal(terra::expanse(masked, transform = FALSE), 4)
  expect_equal(terra::expanse(cropped, transform = FALSE), 16)
})

test_that("prep_landbase does not dissolve when dissolve = none", {
  aoi <- mk_square(0, 4, 0, 4)
  a1 <- mk_square(0, 1, 0, 1)
  a2 <- mk_square(2, 3, 0, 1)
  src <- rbind(a1, a2)
  src$status <- c("Active", "Active")

  out <- prep_landbase(src, aoi, status_col = "status", dissolve = "none")
  expect_equal(nrow(out), 2)
})

test_that("prep_landbase errors informatively on an unknown status column", {
  aoi <- mk_square(0, 4, 0, 4)
  src <- mk_square(0, 2, 0, 2)
  src$status <- "Active"

  expect_snapshot(prep_landbase(src, aoi, status_col = "nope"), error = TRUE)
})

test_that("read_vector_aoi reads a layer with mixed POLYGON/MULTIPOLYGON geometry", {
  ## `sf::st_write()` declares such a layer "Unknown (any)", which terra's proxy reader rejects
  ## outright ("cannot read this geometry type"); the read has to fall back rather than abort.
  skip_if_not_installed("sf")

  sq <- function(x0, y0, w = 1) {
    sf::st_polygon(list(rbind(
      c(x0, y0),
      c(x0 + w, y0),
      c(x0 + w, y0 + w),
      c(x0, y0 + w),
      c(x0, y0)
    )))
  }

  mixed <- sf::st_sf(
    id = c("poly", "multi"),
    value = c(1L, 2L),
    geometry = sf::st_sfc(
      list(sq(0, 0), sf::st_multipolygon(list(list(sq(5, 5)[[1]])))),
      crs = 3005
    )
  )

  gpkg <- file.path(withr::local_tempdir(), "mixed.gpkg")
  sf::st_write(mixed, gpkg, quiet = TRUE)

  ## Whether terra's proxy reader accepts an "Unknown (any)" layer varies with the GDAL build, so
  ## this asserts the outcome rather than which path was taken: the read must return the right
  ## features either way.

  aoi <- terra::vect(sf::st_as_sfc(sf::st_bbox(
    c(xmin = -1, ymin = -1, xmax = 2, ymax = 2),
    crs = 3005
  )))

  out <- read_vector_aoi(gpkg, aoi, fields = "value")

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 1L)
  expect_equal(out$value, 1L)
})

test_that("source_crs falls back to layer metadata when there is no proxy", {
  skip_if_not_installed("sf")

  pts <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = 3005))
  gpkg <- file.path(withr::local_tempdir(), "pts.gpkg")
  sf::st_write(pts, gpkg, quiet = TRUE)

  expect_true(sf::st_crs(source_crs(gpkg, "", proxy = NULL)) == sf::st_crs(3005))
})
