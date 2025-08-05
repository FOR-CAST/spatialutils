#' Convex hull with buffer
#'
#' Draw a convex hull around polygon geometries, creating a single polygon.
#'
#' @param x `sf` polygon object
#'
#' @param d optional buffer distance (m)
#'
#' @return `sf` polygon object
#'
#' @export
outerBuffer <- function(x, d = 0) {
  x |>
    sf::st_union() |>
    sf::st_buffer(d) |>
    sf::st_convex_hull()
}
