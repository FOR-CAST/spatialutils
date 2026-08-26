## x is one 100 x 100 m square; y splits it into a 99 m band and a 1 m sliver.
## min(area) in set "A" is 10,000 m^2, so at areaThresh = 0.05 the cutoff is 500 m^2 and the
## 100 m^2 fragment is a sliver.
ic_fixture <- function() {
  rect <- function(xmin, xmax, ymin, ymax) {
    sf::st_polygon(list(rbind(
      c(xmin, ymin),
      c(xmax, ymin),
      c(xmax, ymax),
      c(xmin, ymax),
      c(xmin, ymin)
    )))
  }

  list(
    x = sf::st_sf(id = "A", geometry = sf::st_sfc(rect(0, 100, 0, 100), crs = 3005)),
    y = sf::st_sf(
      band = c("main", "sliver"),
      geometry = sf::st_sfc(rect(0, 100, 0, 99), rect(0, 100, 99, 100), crs = 3005)
    )
  )
}

area_of <- function(x) sum(as.numeric(sf::st_area(x)))

test_that("slivers are absorbed, not discarded", {
  fx <- ic_fixture()
  out <- intersect_clean(fx$x, fx$y, xcol = "id")

  ## the whole 10,000 m^2 survives -- dropping the sliver would leave 9,900
  expect_equal(area_of(out), area_of(fx$x))
  expect_equal(nrow(out), 1L)
})

test_that("the absorbed sliver takes its neighbour's attributes", {
  fx <- ic_fixture()
  out <- intersect_clean(fx$x, fx$y, xcol = "id")

  expect_equal(out$id, "A")
  expect_equal(out$band, "main") ## not the sliver's spurious "sliver"
})

test_that("nothing is merged when no fragment is below the threshold", {
  fx <- ic_fixture()
  out <- intersect_clean(fx$x, fx$y, xcol = "id", areaThresh = 0)

  expect_equal(nrow(out), 2L)
  expect_setequal(out$band, c("main", "sliver"))
  expect_equal(area_of(out), area_of(fx$x))
})

test_that("feature sets are cleaned independently", {
  fx <- ic_fixture()
  shifted <- fx$x
  sf::st_geometry(shifted) <- sf::st_geometry(shifted) + c(200, 0)
  sf::st_crs(shifted) <- 3005
  shifted$id <- "B"

  y2 <- fx$y
  sf::st_geometry(y2) <- sf::st_geometry(y2) + c(200, 0)
  sf::st_crs(y2) <- 3005

  out <- intersect_clean(rbind(fx$x, shifted), rbind(fx$y, y2), xcol = "id")

  expect_setequal(out$id, c("A", "B"))
  expect_equal(nrow(out), 2L) ## one per set, each having absorbed its own sliver
  expect_equal(area_of(out), 2 * area_of(fx$x))
})
