#' Perimeter of multiple polygons
#'
#' @param x `sf` multipolygons objects
#'
#' @returns `sf` polygon object
#'
#' @export
perimeter <- function(x) {
  sf::st_union(x) |> sf::st_make_valid() |> sf::st_exterior_ring() |> sf::st_as_sf()
}
