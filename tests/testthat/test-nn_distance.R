sq <- function(x0, y0, side = 100) {
  terra::vect(sprintf(
    "POLYGON ((%f %f, %f %f, %f %f, %f %f, %f %f))",
    x0,
    y0,
    x0 + side,
    y0,
    x0 + side,
    y0 + side,
    x0,
    y0 + side,
    x0,
    y0
  ))
}

## A---200---B, A---200---C, D far off to the east (9600 from B)
nn_fixture <- function() {
  v <- rbind(sq(0, 0), sq(300, 0), sq(0, 300), sq(10000, 0))
  terra::crs(v) <- "EPSG:3005"
  v
}

test_that("distances are exact, including past the first search radius", {
  expect_equal(nn_distance(nn_fixture()), c(200, 200, 200, 9600))
})

test_that("touching features are at distance zero", {
  v <- rbind(sq(0, 0), sq(100, 0))
  terra::crs(v) <- "EPSG:3005"

  expect_equal(nn_distance(v), c(0, 0))
})

test_that("the answer does not depend on the search radii supplied", {
  v <- nn_fixture()

  expect_equal(nn_distance(v, radii = 0), nn_distance(v))
  expect_equal(nn_distance(v, radii = c(0, 1e6)), nn_distance(v))
  expect_equal(nn_distance(v, radii = c(0, 10, 20, 40)), nn_distance(v))
})

test_that("a separate `y` measures to that layer, without self-exclusion", {
  x <- sq(0, 0)
  terra::crs(x) <- "EPSG:3005"
  y <- rbind(sq(300, 0), sq(10000, 0))
  terra::crs(y) <- "EPSG:3005"

  expect_equal(nn_distance(x, y), 200)
  ## a feature IS its own neighbour when it comes from the other layer
  expect_equal(nn_distance(x, x), 0)
})

test_that("it agrees with sf::st_nearest_feature", {
  v <- nn_fixture()
  x <- sf::st_as_sf(v)

  expected <- vapply(
    seq_len(nrow(x)),
    function(i) {
      others <- x[-i, ]
      as.numeric(sf::st_distance(x[i, ], others[sf::st_nearest_feature(x[i, ], others), ]))
    },
    numeric(1)
  )

  expect_equal(nn_distance(v), expected)
})

test_that("degenerate inputs give NA rather than an error", {
  one <- sq(0, 0)
  terra::crs(one) <- "EPSG:3005"

  expect_equal(nn_distance(one), NA_real_)
  expect_equal(nn_distance(one, one[0, ]), NA_real_)
  expect_equal(nn_distance(one[0, ]), numeric(0))
})
