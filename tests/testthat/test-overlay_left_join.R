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

olj_base <- function() vect_of("id", c("a", "b"), rect(0, 100, 0, 100), rect(100, 200, 0, 100))

test_that("the left layer's footprint is preserved exactly", {
  ## No area invented, none lost, and none counted twice: `sum(area)` equals the base, and equals
  ## the area of the dissolved result, which is what catches a self-overlapping partition.
  base <- olj_base()
  flag <- vect_of("tag", "t", rect(50, 150, 25, 75))

  out <- overlay_left_join(base, flag)

  expect_equal(area_m2(out), area_m2(base), tolerance = 1e-9)
  expect_equal(area_m2(out), area_m2(terra::aggregate(out)), tolerance = 1e-9)
  expect_named(out, c("id", "tag"))
})

test_that("the right layer's attributes land only where it reaches", {
  base <- olj_base()
  flag <- vect_of("tag", "t", rect(50, 150, 25, 75))

  out <- overlay_left_join(base, flag)
  d <- as.data.frame(out)
  d$m2 <- terra::expanse(out, unit = "m", transform = FALSE)

  expect_equal(sum(d$m2[!is.na(d$tag)]), 100 * 50, tolerance = 1e-9)
  expect_setequal(d$id, c("a", "b"))
})

test_that("a right layer that misses entirely gives NA, and keeps both column sets", {
  base <- olj_base()
  flag <- vect_of("tag", "t", rect(1000, 1100, 1000, 1100))

  out <- overlay_left_join(base, flag)

  expect_equal(area_m2(out), area_m2(base), tolerance = 1e-9)
  expect_named(out, c("id", "tag"))
  expect_true(all(is.na(terra::values(out)$tag)))
})

test_that("an empty right layer gives NA rather than an error", {
  base <- olj_base()
  flag <- vect_of("tag", "t", rect(1000, 1100, 1000, 1100))[integer(0), ]

  out <- overlay_left_join(base, flag)

  expect_equal(area_m2(out), area_m2(base), tolerance = 1e-9)
  expect_named(out, c("id", "tag"))
  expect_true(all(is.na(terra::values(out)$tag)))
})

test_that("a right layer covering everything flags every row", {
  base <- olj_base()
  flag <- vect_of("tag", "t", rect(-10, 210, -10, 110))

  out <- overlay_left_join(base, flag)

  expect_equal(area_m2(out), area_m2(base), tolerance = 1e-9)
  expect_false(any(is.na(terra::values(out)$tag)))
})

test_that("columns come back in the same order whichever branch supplied them", {
  ## `rbind()` on SpatVectors NA-fills a column a branch is missing, silently, so branches that
  ## disagree on their columns produce a quietly wrong answer rather than an error.
  base <- olj_base()
  disjoint <- overlay_left_join(base, vect_of("tag", "t", rect(1000, 1100, 1000, 1100)))
  covered <- overlay_left_join(base, vect_of("tag", "t", rect(-10, 210, -10, 110)))

  expect_identical(names(disjoint), names(covered))
})
