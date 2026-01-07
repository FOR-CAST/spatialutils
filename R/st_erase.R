#' Erase features
#'
#' Erase features `y` from `x`.
#' Adapted from <https://github.com/r-spatial/sf/issues/346>.
#'
#' @param x,y sf polygon objects
#'
#' @returns sf polygon object
st_erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(y)) |> sf::st_make_valid()
}
