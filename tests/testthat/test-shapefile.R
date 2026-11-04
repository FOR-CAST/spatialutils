test_that("shapefile_counts reads the shape and record counts from the file headers", {
  shp <- local_truncated_shapefile(n = 5L)
  expect_identical(shapefile_counts(shp), c(shapes = 5, records = 5))

  ## a cut .dbf whose header still claims every record: the file size gives the true count
  cut <- local_truncated_shapefile(n = 5L, keep = 3L)
  expect_identical(shapefile_counts(cut), c(shapes = 5, records = 3))
})

test_that("shapefile_counts returns NULL when it cannot read both sidecar files", {
  shp <- local_truncated_shapefile(n = 2L)
  file.remove(sub("\\.shp$", ".dbf", shp))
  expect_null(shapefile_counts(shp))

  expect_null(shapefile_counts("/vsizip/archive.zip/landbase.shp"))
  expect_null(shapefile_counts(file.path(withr::local_tempdir(), "layer.gpkg")))
})

test_that("stop_if_dbf_truncated names both counts", {
  shp <- local_truncated_shapefile(n = 3L, keep = 2L)
  expect_error(stop_if_dbf_truncated(shp), "2 complete records.*3 shapes")
  expect_no_error(stop_if_dbf_truncated(local_truncated_shapefile(n = 3L)))
})

test_that("read_vector reads an intact shapefile in full and stops on a cut one", {
  expect_equal(nrow(read_vector(local_truncated_shapefile(n = 3L))), 3L)

  ## guards the fixture: read directly, the third shape is lost with only a warning
  shp <- local_truncated_shapefile(n = 3L, keep = 2L)
  expect_equal(nrow(suppressWarnings(terra::vect(shp))), 2L)

  expect_error(read_vector(shp), "2 complete records.*3 shapes")
})

test_that("read_vector stops on GDAL's .dbf read failure when it cannot count records", {
  shp <- local_truncated_shapefile(n = 3L, keep = 2L)
  local_mocked_bindings(shapefile_counts = function(...) NULL)

  expect_error(read_vector(shp), "failed on DBF file")
})

test_that("read_vector passes its other arguments to terra::vect", {
  shp <- local_truncated_shapefile(n = 3L)
  v <- read_vector(shp, query = "SELECT * FROM landbase WHERE status = 'Active'")
  expect_equal(nrow(v), 2L)
})
