sq <- function(x0, y0, s = 100) {
  terra::vect(
    sprintf(
      "POLYGON ((%1$s %2$s, %3$s %2$s, %3$s %4$s, %1$s %4$s, %1$s %2$s))",
      x0,
      y0,
      x0 + s,
      y0 + s
    ),
    crs = "EPSG:3005"
  )
}

test_that("repair_geoms() repairs the invalid subset instead of dropping it", {
  bowtie <- terra::vect(
    "POLYGON ((0 0, 200 200, 0 200, 200 0, 0 0))", # self-intersecting -> invalid
    crs = "EPSG:3005"
  )
  v <- rbind(sq(0, 0), sq(120, 0), bowtie)
  v$id <- c(1L, 2L, 3L)
  expect_equal(sum(!terra::is.valid(v)), 1L)

  out <- repair_geoms(v)
  expect_equal(nrow(out), 3L) # recovered, not dropped
  expect_equal(sum(terra::is.valid(out)), 3L) # all valid after repair
  expect_setequal(out$id, c(1L, 2L, 3L)) # attributes preserved
})

test_that("repair_geoms() leaves an all-valid layer intact", {
  v <- rbind(sq(0, 0), sq(120, 0))
  expect_equal(nrow(repair_geoms(v)), 2L)
})

test_that("repair_geoms() coerces a non-SpatVector input", {
  out <- repair_geoms("POLYGON ((0 0, 200 200, 0 200, 200 0, 0 0))")
  expect_s4_class(out, "SpatVector")
  expect_equal(sum(terra::is.valid(out)), nrow(out))
})

test_that("repair_geoms() drops a ring that collapses to a line instead of failing", {
  ## `makeValid()` turns a zero-area ring into a LINESTRING. When every invalid geometry is one,
  ## the repaired set comes back as lines, and `rbind()` with the valid polygons used to stop with
  ## "all SpatVectors must have the same geometry type".
  collapsed <- terra::vect("POLYGON ((300 0, 310 0, 320 0, 300 0))", crs = "EPSG:3005")
  v <- rbind(sq(0, 0), sq(120, 0), collapsed)
  v$id <- c(1L, 2L, 3L)
  expect_equal(sum(!terra::is.valid(v)), 1L)

  out <- repair_geoms(v)
  expect_equal(terra::geomtype(out), "polygons")
  expect_equal(out$id, c(1L, 2L))
})
