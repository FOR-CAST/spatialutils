#' Drop all attributes from a vector layer, keeping its geometries
#'
#' `terra` has no equivalent of `x[, character(0)]`: subsetting a `SpatVector`
#' to zero columns is an error, and `x$col <- NULL` has to be repeated for every
#' column. This assigns an empty attribute table instead.
#'
#' Useful when a layer is only wanted for its geometry -- as the `y` of an
#' [terra::erase()], for instance, where the attributes are irrelevant and
#' carrying them only bloats the intermediate.
#'
#' @param x A `SpatVector`, or an object coercible to one via [terra::vect()].
#'
#' @return A `SpatVector` with the same geometries and no attributes.
#'
#' @family SpatVector helpers
#' @export
#'
#' @examples
#' v <- terra::vect("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' v$keep_me <- "no"
#' names(drop_values(v))
drop_values <- function(x) {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }

  terra::values(x) <- data.frame(matrix(nrow = nrow(x), ncol = 0))

  x
}

#' Planar area of a vector layer
#'
#' Area measured in the layer's own projection, which is what [sf::st_area()]
#' returns and what ArcGIS reports as `Shape_Area`.
#'
#' [terra::expanse()] defaults to `transform = TRUE`, which reprojects to
#' lon/lat and returns *geodesic* area instead. The two agree in an equal-area
#' projection (e.g. BC Albers) and disagree everywhere else: in Canada Atlas
#' Lambert at BC latitudes geodesic area runs about 2.8% high, which is enough
#' to move features across an area threshold and to shift every reported total.
#' Prefer this wherever an area has to line up with `sf`, with ArcGIS, or with a
#' fixed cutoff.
#'
#' For a layer in a geographic (lon/lat) CRS, planar area is meaningless and
#' `terra` returns geodesic area regardless; this warns in that case.
#'
#' @param x A `SpatVector`, or an object coercible to one via [terra::vect()].
#' @param unit Character. Output unit, passed to [terra::expanse()]: one of
#'   `"m"` (the default), `"km"`, `"ha"`.
#'
#' @return A numeric vector of areas in `unit`, one per feature.
#'
#' @family SpatVector helpers
#' @seealso [terra::expanse()]
#' @export
#'
#' @examples
#' v <- terra::vect("POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))")
#' terra::crs(v) <- "EPSG:3005"
#' expanse_planar(v, "m")
expanse_planar <- function(x, unit = "m") {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }

  if (isTRUE(terra::is.lonlat(x, warn = FALSE))) {
    warning(
      "`x` has a geographic CRS; planar area is not meaningful and `terra` returns geodesic ",
      "area regardless. Project to a projected CRS first."
    )
  }

  terra::expanse(x, unit = unit, transform = FALSE)
}

#' Dissolve a vector layer on one or more attributes
#'
#' Merges features that share the same values of `by` into a single geometry,
#' then (by default) splits multipart results back to single-part features.
#'
#' This exists because [terra::aggregate()] cannot dissolve on **several**
#' columns when any of them contains `NA`: it returns a `SpatVector` whose
#' attribute table has fewer rows than it has geometries, which then errors on
#' the next access (`nrow dataframe does not match nrow geometry`). An
#' unclassified category is usually still a category -- an unclassified seral
#' stage, an unmapped landbase status -- so dropping or corrupting it is rarely
#' what a dissolve is meant to do. Here `NA` values are encoded to a sentinel
#' before aggregating and restored afterwards, so every `NA` group survives as a
#' group of its own.
#'
#' The `agg_n` bookkeeping column [terra::aggregate()] adds is removed.
#'
#' @param x A `SpatVector`, or an object coercible to one via [terra::vect()].
#' @param by Character vector of attribute names to dissolve on.
#' @param explode Logical. If `TRUE` (the default), split multipart geometries
#'   back to single-part with [terra::disagg()].
#'
#' @return A `SpatVector` with one (or, when `explode = TRUE`, one or more)
#'   feature per combination of `by` values present in `x`, carrying only the
#'   `by` columns.
#'
#' @family SpatVector helpers
#' @export
#'
#' @examples
#' v <- terra::vect(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
#'   "POLYGON ((3 0, 4 0, 4 1, 3 1, 3 0))"
#' ))
#' v$class <- c("a", "a", NA)
#'
#' ## the NA feature survives as its own group
#' as.data.frame(dissolve_by(v, "class"))
dissolve_by <- function(x, by, explode = TRUE) {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }
  stopifnot(is.character(by), length(by) > 0)

  missing_cols <- setdiff(by, names(x))
  if (length(missing_cols) > 0) {
    stop("column(s) not found in `x`: ", paste(missing_cols, collapse = ", "))
  }

  ## a sentinel no real value can collide with
  na_token <- "\001NA\001"

  x <- x[, by]

  for (nm in by) {
    values <- as.character(terra::values(x)[[nm]])
    x[[nm]] <- ifelse(is.na(values), na_token, values)
  }

  out <- terra::aggregate(x, by = by)

  if ("agg_n" %in% names(out)) {
    out$agg_n <- NULL
  }

  if (isTRUE(explode)) {
    out <- terra::disagg(out)
  }

  for (nm in by) {
    values <- terra::values(out)[[nm]]
    out[[nm]] <- ifelse(values == na_token, NA_character_, values)
  }

  out
}
