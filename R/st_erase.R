#' Erase features
#'
#' Erase features `y` from `x`.
#' Adapted from <https://github.com/r-spatial/sf/issues/346>.
#'
#' @param x,y sf polygon objects
#'
#' @returns sf polygon object
st_erase <- function(x, y) {
  ## `sf::st_union()` of an empty layer is a zero-length geometry, and `st_difference()` against
  ## that fails with "replacement has 1 row, data has 0". Nothing to erase means `x` comes back
  ## whole.
  if (nrow(y) == 0L) {
    return(x)
  }

  sf::st_difference(x, sf::st_union(y)) |> sf::st_make_valid() |> keep_polygons()
}
