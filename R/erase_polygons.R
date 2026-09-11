#' Erase one polygon layer from another, keeping attributes aligned
#'
#' The part of `x` that lies outside `y`, keeping `x`'s attributes: every feature of `x` comes
#' back with whatever part of it lies outside `y`.
#'
#' @section Why this goes through sf rather than terra:
#' `terra::erase()` can return a `SpatVector` carrying one more attribute row
#' than it has geometries: it drops a geometry whose difference comes out empty
#' without dropping the matching attribute row. Nothing complains at the time,
#' and the next thing to read the attributes fails instead, a long way from the
#' cause -- one pipeline run got six hours in before dying on
#' `[[<-,SpatVector] cannot add these values`. Measured on a real seral overlay
#' it hit 5 of 49 tiles, each off by exactly one row.
#'
#' Reported as <https://github.com/rspatial/terra/issues/2179>, open at the time
#' of writing, and *not* covered by the fix for
#' <https://github.com/rspatial/terra/issues/2175> -- that moved `union()` onto
#' `erase_agg()`, which is the function `erase()` already called.
#'
#' Going through `sf` cannot desynchronise, because the attributes are columns
#' of the same data frame as the geometry. Measured across 42 tiles of a study
#' area, `sf` was consistent on all 42 where terra managed 37, and where terra
#' did succeed the two agreed to 0.000000 m², at about twice the cost.
#'
#' @param x `SpatVector` polygons to erase from.
#' @param y `SpatVector` polygons to erase. May be empty, in which case `x` is
#'   returned unchanged -- `sf::st_union()` of an empty layer is a zero-length
#'   geometry, and `sf::st_difference()` against that fails with a message that
#'   says nothing about the cause.
#'
#' @return `SpatVector` polygons carrying `x`'s attributes, always with exactly
#'   as many attribute rows as geometries. Empty, but with `x`'s columns intact,
#'   when `y` covers `x` completely.
#'
#' @seealso [keep_polygons()], [overlay_left_join()]
#' @export
erase_polygons <- function(x, y) {
  UseMethod("erase_polygons")
}

#' @rdname erase_polygons
#' @export
erase_polygons.sf <- function(x, y) {
  if (!inherits(y, "sf")) {
    y <- sf::st_as_sf(y)
  }

  if (nrow(y) == 0L) {
    return(x)
  }

  suppressWarnings(sf::st_difference(x, sf::st_union(y))) |> keep_polygons()
}

#' @rdname erase_polygons
#' @export
erase_polygons.SpatVector <- function(x, y) {
  if (nrow(y) == 0L) {
    return(x)
  }

  d <- erase_polygons(sf::st_as_sf(x), sf::st_as_sf(y))

  ## `[` keeps the columns, where `terra::crop()` would drop them, and
  ## `terra::vect()` warns on an empty `sf`.
  if (nrow(d) == 0L) {
    return(x[integer(0), ])
  }

  terra::vect(d)
}

## Overlays can return a `SpatVector` whose attribute table has a different
## number of rows from its geometry. Nothing complains at the time; the next
## thing to touch the attributes fails instead, a long way from the cause.
assert_synced <- function(v, what) {
  if (nrow(v) != nrow(terra::values(v))) {
    stop(sprintf(
      "%s came back with %d geometries but %d attribute rows",
      what,
      nrow(v),
      nrow(terra::values(v))
    ))
  }

  v
}

#' @param tiles `SpatVector` or `sf` of tile polygons covering `x`. Reuse an existing grid where one
#'   exists -- see the note in `Details`.
#'
#' @details
#' `erase_polygons_tiled()` is the same operation done tile by tile. It exists because the cost of a
#' difference is superlinear in the complexity of `y`: when `y` is one large dissolved multipolygon,
#' every feature of `x` pays for district-wide geometry no matter how small it is. Cropping both
#' sides to a tile shrinks the `y` each difference sees.
#'
#' Measured on a 40,301-polygon layer differenced against a single 1.88 Mha multipolygon:
#' 11,157 s untiled against 201.8 s over 56 tiles, a 55.3x reduction, with identical output. On a
#' second, larger study area the same change took a target from 27.6 h to 35 min (46.7x), again
#' producing bit-identical geometry -- 32,305 features and 513,509.0693 ha either way.
#'
#' It is not always a win. Tiling adds a crop per tile, so it pays only when `y` is complex enough
#' that shrinking it saves more than the crops cost. Measure before adopting it.
#'
#' Geometries crossing a tile seam come back SPLIT. Dissolve after this returns, never per tile, if
#' they need rejoining -- a dissolve that sees only one half of a split feature cannot rejoin it.
#' Reuse an existing grid where the layer was built on one: it already carries vertices lying
#' exactly on those seam coordinates, so re-cutting adds no new vertices and the halves abut exactly.
#'
#' @rdname erase_polygons
#' @export
erase_polygons_tiled <- function(x, y, tiles) {
  if (nrow(y) == 0L) {
    return(x)
  }

  overlay_tiled(x, y, tiles, function(xi, yi) {
    if (nrow(yi) == 0L) xi else erase_polygons(xi, yi)
  })
}
