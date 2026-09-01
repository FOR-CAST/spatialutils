#' Keep only the polygonal part of an overlay result
#'
#' An overlay can leave a line or a point behind: inside a `GEOMETRYCOLLECTION`,
#' or -- where everything left of a polygon is a shared edge -- as a bare
#' `LINESTRING` sitting directly in the geometry column. Only the polygons carry
#' area, and the residue causes trouble downstream out of all proportion to the
#' area it represents, which is none.
#'
#' @section Why testing for `GEOMETRYCOLLECTION` is not enough:
#' Two polygons that share only an edge intersect in a bare `LINESTRING`, with
#' no collection anywhere in the result, so a guard written as
#' `if (any(st_geometry_type(x) == "GEOMETRYCOLLECTION"))` never fires.
#'
#' The way that fails is worth spelling out. [terra::vect()] **drops** a
#' non-polygon geometry while keeping its attribute row, so the `SpatVector`
#' comes back carrying more attribute rows than it has geometries. Nothing
#' complains at the time; the next thing to touch the attributes fails instead,
#' a long way from the cause. Measured on one tile of a real study area, exactly
#' one of 6,871 differences was such a line, and it surfaced two function calls
#' later as `[as,sf] coercion failed`.
#'
#' Filtering in `sf` is what makes this safe: attributes are columns of the same
#' data frame as the geometry, so they cannot desynchronise.
#'
#' @param x `sf` object
#'
#' @return `sf` object holding only non-empty `POLYGON` / `MULTIPOLYGON`
#'   features. May have **more** rows than `x`, when a collection held several
#'   polygons, and fewer when a feature had no polygonal part at all.
#'
#' @seealso [erase_polygons()], [overlay_left_join()], [intersect_clean()]
#' @export
keep_polygons <- function(x) {
  polygonal <- c("POLYGON", "MULTIPOLYGON")
  types <- as.character(sf::st_geometry_type(x))

  if (all(types %in% polygonal) && !any(sf::st_is_empty(x))) {
    return(x)
  }

  ## `st_collection_extract()` errors outright when every row is a single
  ## non-polygon type ("x is of singular geometry type that is different to
  ## supplied type"), which is exactly what an overlay reduced entirely to lines
  ## looks like -- so it runs only when there is a collection to extract from.
  if (any(types == "GEOMETRYCOLLECTION")) {
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
    types <- as.character(sf::st_geometry_type(x))
  }

  x[types %in% polygonal & !sf::st_is_empty(x), , drop = FALSE]
}
