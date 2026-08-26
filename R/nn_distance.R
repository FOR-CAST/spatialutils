#' Distance from each feature to its nearest neighbour
#'
#' Exact distance from every feature of `x` to the nearest feature of `y` --
#' or, when `y` is `NULL`, to the nearest *other* feature of `x`.
#'
#' The obvious implementation, calling [sf::st_nearest_feature()] once per
#' feature against everything else, rebuilds the GEOS index on every call. On a
#' 70,000-polygon layer that costs seconds *per polygon*. This instead runs one
#' indexed query per round over all outstanding features at once, and computes
#' exact geometry-to-geometry distances only for the candidates that query
#' returns.
#'
#' Rounds proceed from the smallest radius up:
#'
#' * radius `0` is a plain intersects test, so touching or overlapping features
#'   are resolved at distance 0 with no geometry work at all;
#' * each later round buffers only the features still unresolved and looks for
#'   candidates within that radius.
#'
#' Most features resolve in the first round or two, so the expensive exact
#' distance step sees a handful of nearby candidates rather than everything
#' within one generous fixed radius.
#'
#' A buffered polygon approximates a circle with straight segments and therefore
#' sits slightly *inside* the true circle. A distance found near the rim of the
#' search radius might have a nearer neighbour in the sliver the approximation
#' cut off, so only distances comfortably inside the radius are accepted; the
#' rest fall through to the next, larger round.
#'
#' @param x A `SpatVector`, or an object coercible to one via [terra::vect()].
#' @param y Optional `SpatVector` to measure distances to. When `NULL` (the
#'   default), distances are measured within `x`, excluding each feature from
#'   its own candidate set.
#' @param radii Numeric vector of search radii in map units, smallest first. The
#'   default steps from touching (`0`) up by factors of four. A final radius
#'   spanning the whole extent is always appended, so the search cannot run out
#'   of rounds while a neighbour still exists.
#'
#' @return A numeric vector of distances, one per feature of `x`, in map units.
#'   `NA` for a feature with no neighbour at all (only possible when `x` has a
#'   single feature and `y` is `NULL`, or `y` is empty).
#'
#' @family SpatVector helpers
#' @export
#'
#' @examples
#' squares <- terra::vect(c(
#'   "POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))",
#'   "POLYGON ((300 0, 400 0, 400 100, 300 100, 300 0))",
#'   "POLYGON ((0 300, 100 300, 100 400, 0 400, 0 300))"
#' ))
#' terra::crs(squares) <- "EPSG:3005"
#' nn_distance(squares)
nn_distance <- function(x, y = NULL, radii = c(0, 250, 1000, 4000, 16000, 64000)) {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }

  self <- is.null(y)
  if (self) {
    y <- x
  } else if (!inherits(y, "SpatVector")) {
    y <- terra::vect(y)
  }

  out <- rep(NA_real_, nrow(x))

  if (nrow(x) == 0L || nrow(y) == 0L || (self && nrow(x) < 2L)) {
    return(out)
  }

  ## Always finish with a radius that spans the data, so a neighbour that exists is always found.
  extent <- terra::ext(rbind(terra::as.polygons(terra::ext(x)), terra::as.polygons(terra::ext(y))))
  span <- sqrt(
    (terra::xmax(extent) - terra::xmin(extent))^2 + (terra::ymax(extent) - terra::ymin(extent))^2
  )
  radii <- sort(unique(c(radii[radii < span], span)))

  todo <- seq_len(nrow(x))

  for (radius in radii) {
    if (length(todo) == 0L) {
      break
    }

    query <- if (radius > 0) terra::buffer(x[todo, ], width = radius) else x[todo, ]
    candidates <- terra::relate(query, y, "intersects", pairs = TRUE)

    if (nrow(candidates) > 0L && self) {
      ## a feature is not its own neighbour
      candidates <- candidates[todo[candidates[, 1]] != candidates[, 2], , drop = FALSE]
    }

    if (nrow(candidates) > 0L) {
      if (radius == 0) {
        out[todo[unique(candidates[, 1])]] <- 0
      } else {
        distances <- terra::distance(
          x[todo[candidates[, 1]], ],
          y[candidates[, 2], ],
          pairwise = TRUE
        )
        nearest <- tapply(distances, candidates[, 1], min)
        position <- as.integer(names(nearest))

        ## only trust a distance comfortably inside the (polygon-approximated) search radius
        trusted <- nearest <= radius * 0.99 | radius >= span
        out[todo[position[trusted]]] <- nearest[trusted]
      }
    }

    todo <- todo[is.na(out[todo])]
  }

  out
}
