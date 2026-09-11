#' Symmetric difference of two polygon layers
#'
#' The parts of `x` and `y` that do not overlap: `(x \ y)` together with `(y \ x)`. Built from two
#' [erase_polygons()] calls, so it inherits the same guards -- the empty-layer case, the
#' non-polygonal residue an overlay leaves behind, and row alignment.
#'
#' @section Why the result carries no attributes:
#' The two halves come from different layers, which in general have different columns. `rbind()` on
#' `SpatVector`s matches columns by NAME and silently NA-fills any a side is missing, so a naive
#' combine reports "this attribute is NA here" where the truth is "this attribute does not exist on
#' this side" -- indistinguishable from a real result, and wrong in the quiet direction. Rather than
#' guess a reconciliation, this returns geometry plus a `source` column saying which layer each part
#' came from. Join attributes back yourself if you need them.
#'
#' @param x,y `SpatVector` polygon layers. Either may be empty, in which case the other is returned
#'   whole with its `source` tag.
#'
#' @return `SpatVector` polygons with a single character column, `source`, valued `"x"` or `"y"`.
#'
#' @seealso [erase_polygons()], [keep_polygons()]
#' @family spatial helpers
#' @export
sym_difference <- function(x, y) {
  UseMethod("sym_difference")
}

#' @rdname sym_difference
#' @export
sym_difference.sf <- function(x, y) {
  if (!inherits(y, "sf")) {
    y <- sf::st_as_sf(y)
  }

  x_only <- if (nrow(y) == 0L) x else erase_polygons(x, y)
  y_only <- if (nrow(x) == 0L) y else erase_polygons(y, x)

  ## geometry only -- see "Why the result carries no attributes"
  x_only <- sf::st_sf(source = rep("x", nrow(x_only)), geometry = sf::st_geometry(x_only))
  y_only <- sf::st_sf(source = rep("y", nrow(y_only)), geometry = sf::st_geometry(y_only))

  if (nrow(x_only) == 0L) {
    return(y_only)
  }
  if (nrow(y_only) == 0L) {
    return(x_only)
  }

  rbind(x_only, y_only)
}

#' @rdname sym_difference
#' @export
sym_difference.SpatVector <- function(x, y) {
  x_only <- if (nrow(y) == 0L) x else erase_polygons(x, y)
  y_only <- if (nrow(x) == 0L) y else erase_polygons(y, x)

  x_only <- drop_values(x_only)
  y_only <- drop_values(y_only)

  if (nrow(x_only) > 0L) {
    x_only$source <- "x"
  }
  if (nrow(y_only) > 0L) {
    y_only$source <- "y"
  }

  if (nrow(x_only) == 0L) {
    return(y_only)
  }
  if (nrow(y_only) == 0L) {
    return(x_only)
  }

  rbind(x_only, y_only)
}

#' @param tiles `SpatVector` or `sf` of tile polygons covering both layers.
#'
#' @details
#' `sym_difference_tiled()` does the same tile by tile, for the same reason as
#' [erase_polygons_tiled()]: difference cost is superlinear in the complexity of the layer being
#' subtracted. Geometries crossing a seam come back SPLIT; dissolve after this returns, never per
#' tile. Measure before adopting -- tiling adds a crop per tile and only pays when the layers are
#' complex enough for that to be worth it.
#'
#' @rdname sym_difference
#' @export
sym_difference_tiled <- function(x, y, tiles) {
  overlay_tiled(x, y, tiles, function(xi, yi) sym_difference(xi, yi))
}
