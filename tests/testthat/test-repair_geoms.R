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
