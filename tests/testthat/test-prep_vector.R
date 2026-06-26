test_that("prep_vector clips to the study-area polygon and keeps x attributes", {
  studyArea <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  terra::crs(studyArea) <- "EPSG:3857"
  x <- terra::vect("POLYGON ((8 4, 12 4, 12 6, 8 6, 8 4))")
  terra::crs(x) <- "EPSG:3857"
  x$id <- "straddle"

  out <- prep_vector(x, studyArea)
  expect_s4_class(out, "SpatVector")
  expect_identical(names(out), "id")
  expect_equal(max(terra::geom(out)[, "x"]), 10)
})

test_that("prep_vector reprojects to the requested crs", {
  studyArea <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  terra::crs(studyArea) <- "EPSG:3857"
  x <- terra::vect("POLYGON ((4 4, 6 4, 6 6, 4 6, 4 4))")
  terra::crs(x) <- "EPSG:3857"

  out <- prep_vector(x, studyArea, crs = "EPSG:4326")
  expect_equal(terra::crs(out, describe = TRUE)$code, "4326")
})

test_that("prep_vector mask = FALSE crops to extent, mask = TRUE clips to polygon", {
  studyArea <- terra::vect("POLYGON ((0 0, 10 0, 0 10, 0 0))")
  terra::crs(studyArea) <- "EPSG:3857"
  x <- terra::vect("POLYGON ((7 7, 9 7, 9 9, 7 9, 7 7))")
  terra::crs(x) <- "EPSG:3857"

  expect_equal(nrow(prep_vector(x, studyArea, mask = FALSE)), 1)
  expect_equal(nrow(prep_vector(x, studyArea, mask = TRUE)), 0)
})

test_that("prep_vector returns valid geometry from invalid input", {
  studyArea <- terra::vect("POLYGON ((-1 -1, 3 -1, 3 3, -1 3, -1 -1))")
  terra::crs(studyArea) <- "EPSG:3857"
  bowtie <- terra::vect("POLYGON ((0 0, 2 2, 2 0, 0 2, 0 0))")
  terra::crs(bowtie) <- "EPSG:3857"

  expect_equal(all(terra::is.valid(prep_vector(bowtie, studyArea))), TRUE)
})

test_that("prep_vector returns empty (no error) when x does not overlap studyArea", {
  studyArea <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  terra::crs(studyArea) <- "EPSG:3857"
  far <- terra::vect("POLYGON ((100 100, 110 100, 110 110, 100 110, 100 100))")
  terra::crs(far) <- "EPSG:3857"

  out <- prep_vector(far, studyArea)
  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 0L)
})
