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
  if (nrow(y) == 0L) {
    return(x)
  }

  d <- suppressWarnings(sf::st_difference(sf::st_as_sf(x), sf::st_union(sf::st_as_sf(y)))) |>
    keep_polygons()

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
