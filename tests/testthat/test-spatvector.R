mk_poly <- function(xmin, xmax, ymin, ymax, crs = "EPSG:3005") {
  v <- terra::vect(sprintf(
    "POLYGON ((%f %f, %f %f, %f %f, %f %f, %f %f))",
    xmin,
    ymin,
    xmax,
    ymin,
    xmax,
    ymax,
    xmin,
    ymax,
    xmin,
    ymin
  ))
  terra::crs(v) <- crs
  v
}

test_that("drop_values keeps geometries and removes every attribute", {
  v <- mk_poly(0, 1, 0, 1)
  v$a <- "x"
  v$b <- 1

  out <- drop_values(v)

  expect_s4_class(out, "SpatVector")
  expect_equal(names(out), character(0))
  expect_equal(nrow(out), nrow(v))
  expect_equal(terra::expanse(out, transform = FALSE), terra::expanse(v, transform = FALSE))
})

test_that("expanse_planar matches sf::st_area and not the geodesic default", {
  ## Canada Atlas Lambert is conformal, so geodesic and planar area differ
  v <- mk_poly(-1800000, -1799900, 850000, 850100, crs = "EPSG:3979")

  expect_equal(expanse_planar(v, "m"), 10000)
  expect_equal(expanse_planar(v, "m"), as.numeric(sf::st_area(sf::st_as_sf(v))))
  expect_gt(terra::expanse(v, unit = "m"), expanse_planar(v, "m"))
})

test_that("expanse_planar warns for a geographic CRS", {
  expect_snapshot(x <- expanse_planar(mk_poly(0, 1, 0, 1, crs = "EPSG:4326")))
})

test_that("dissolve_by merges features sharing a value", {
  v <- rbind(mk_poly(0, 1, 0, 1), mk_poly(1, 2, 0, 1), mk_poly(3, 4, 0, 1))
  v$class <- c("a", "a", "b")

  out <- dissolve_by(v, "class", explode = FALSE)

  expect_setequal(out$class, c("a", "b"))
  expect_equal(nrow(out), 2L)
  expect_equal(
    sum(terra::expanse(out, transform = FALSE)),
    sum(terra::expanse(v, transform = FALSE))
  )
})

test_that("dissolve_by keeps NA as a group of its own", {
  v <- rbind(mk_poly(0, 1, 0, 1), mk_poly(3, 4, 0, 1))
  v$class <- c("a", NA)

  out <- dissolve_by(v, "class")

  expect_equal(nrow(out), 2L)
  expect_equal(sum(is.na(out$class)), 1L)
  expect_equal(
    sum(terra::expanse(out, transform = FALSE)),
    sum(terra::expanse(v, transform = FALSE))
  )
})

test_that("dissolve_by handles several `by` columns containing NA", {
  ## terra::aggregate() returns a CORRUPT SpatVector here -- its attribute table has fewer rows
  ## than it has geometries -- which errors on the next access rather than at the aggregate call
  v <- rbind(mk_poly(0, 1, 0, 1), mk_poly(3, 4, 0, 1), mk_poly(6, 7, 0, 1))
  v$class <- c("a", NA, "a")
  v$sub <- c("p", "p", NA)

  expect_error(as.data.frame(terra::aggregate(v, by = c("class", "sub"))))

  out <- dissolve_by(v, c("class", "sub"))

  expect_equal(nrow(out), 3L)
  expect_equal(
    sum(terra::expanse(out, transform = FALSE)),
    sum(terra::expanse(v, transform = FALSE))
  )
  expect_equal(sum(is.na(out$class)), 1L)
  expect_equal(sum(is.na(out$sub)), 1L)
})

test_that("dissolve_by explodes multipart results by default", {
  v <- rbind(mk_poly(0, 1, 0, 1), mk_poly(3, 4, 0, 1))
  v$class <- "a"

  expect_equal(nrow(dissolve_by(v, "class")), 2L)
  expect_equal(nrow(dissolve_by(v, "class", explode = FALSE)), 1L)
})

test_that("dissolve_by keeps only the `by` columns and drops agg_n", {
  v <- rbind(mk_poly(0, 1, 0, 1), mk_poly(1, 2, 0, 1))
  v$class <- "a"
  v$other <- "dropped"

  expect_equal(names(dissolve_by(v, "class")), "class")
})

test_that("dissolve_by rejects unknown columns", {
  v <- mk_poly(0, 1, 0, 1)
  v$class <- "a"
  expect_snapshot(dissolve_by(v, "nope"), error = TRUE)
})
