poly_sf <- function() {
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      crs = 4326
    )
  )
}

test_that("create_bbox returns a bbox matching the geometry", {
  bb <- create_bbox(poly_sf())
  expect_s3_class(bb, "bbox")
  expect_equal(as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")]), c(0, 0, 2, 2))
})

test_that("calc_centroid returns numeric centroid coordinates", {
  cc <- calc_centroid(poly_sf())
  expect_type(cc, "double")
  expect_equal(cc, c(1, 1), tolerance = 1e-3)
})

test_that("extract_coords returns a plain numeric coordinate vector", {
  pt <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(3, 4)), crs = 4326))
  xy <- extract_coords(pt)
  expect_type(xy, "double")
  expect_equal(xy, c(3, 4))
})

test_that("spatial_join_intersects subsets to intersecting features", {
  grid <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)))),
      crs = 4326
    )
  )
  pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), crs = 4326))
  res <- spatial_join_intersects(grid, pt)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1L)
  expect_equal(res$id, 1L)
})

test_that("save_gpkg round-trips an sf object", {
  pts <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  )
  dst <- withr::local_tempfile(fileext = ".gpkg")
  out <- save_gpkg(pts, dst)
  expect_equal(out, dst)

  back <- sf::st_read(dst, quiet = TRUE)
  expect_equal(nrow(back), nrow(pts))
  expect_equal(as.character(unique(sf::st_geometry_type(back))), "POINT")
})

test_that("prep_agg_lonlat_raster reprojects and aggregates", {
  r <- terra::rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100, resolution = 10, crs = "epsg:3005")
  terra::values(r) <- seq_len(terra::ncell(r))
  res <- prep_agg_lonlat_raster(terra::ext(r), r, agg_to = 50)
  expect_s4_class(res, "SpatRaster")
  expect_equal(terra::is.lonlat(res), TRUE)
})
