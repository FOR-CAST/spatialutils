#' Intersect and cleanup crumbs
#'
#' Useful in cases where two sets of polygons do not align perfectly spatially.
#' Thus, intersecting produces slivers that are larger than can typically
#' be easily dealt with (e.g., <https://github.com/r-spatial/sf/issues/547>).
#'
#' In these cases, deal with each of the set of features individually:
#' 1. Identify the feature sets of interest in `x` using the unique values of `x[[xcol]]`
#'    (i.e., sets of polygons that share an identifier);
#' 2. For each of the feature sets, calculate the area of the smallest feature in that set;
#' 3. For each of the feature sets, perform intersection with `y` and ensure validity;
#' 4. Post-intersection, remove any fragment polygons smaller than `areaThresh * min(area)`.
#'
#' @param x,y `sf` polygons object
#' @param xcol character, name of the attribute column in `x` to use to identify features
#' @param areaThresh numeric, *proportion* of minimum polygon area to use as threshold crumb size.
#'
#' @return `sf` polygons object
#'
#' @export
intersect_clean <- function(x, y, xcol, areaThresh = 0.05) {
  xy <- sf::st_intersection(x, y) |>
    sf::st_make_valid()
  names.x <- unique(xy[[xcol]])
  areas.x <- lapply(names.x, function(p) {
    sf::st_area(x[x[[xcol]] == p, ])
  })
  names(areas.x) <- names.x

  z <- lapply(names.x, function(p, polys, areas) {
    polys[polys[[xcol]] == p, ] |>
      sf::st_collection_extract("POLYGON") |>
      smoothr::drop_crumbs(areaThresh * min(areas[[p]])) |>
      sf::st_make_valid()
  }, polys = xy, areas = areas.x)
  z <- do.call(rbind, z)
  z <- z[!sf::st_is_empty(z), ]

  return(z)
}
