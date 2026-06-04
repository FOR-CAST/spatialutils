#' Eliminate sliver polygons by merging them into a neighbour
#'
#' Emulates the ArcGIS `Eliminate` tool (the *LENGTH* option): small "sliver"
#' polygons are removed by merging each into the neighbouring polygon with
#' which it shares the longest border. This is the geometrically-sensible way
#' to clean up the slivers left behind when two polygon layers that do not
#' align (e.g. Landscape Units vs. Natural Resource Districts) are intersected,
#' and is preferable to area-thresholding crumbs away (see [intersect_clean()]).
#'
#' The merge is delegated to [terra::combineGeoms()]; the defaults here
#' (`boundary = TRUE`, `overlap = FALSE`) give pure longest-shared-border
#' assignment. `distance`/`maxdist` only come into play for slivers that share
#' no edge with any keeper (e.g. they touch at a point, or sit in a small gap).
#'
#' `x` is returned as the same class it was passed in as ("return the type
#' that was passed"): an `sf` object in yields an `sf` object out, a
#' `SpatVector` in yields a `SpatVector` out. Internally the work is done with
#' \pkg{terra}.
#'
#' Slivers are merged into keepers, so the **attributes of the keeper are
#' retained** and a sliver's own attributes are discarded (as in ArcGIS
#' `Eliminate`). Use `keep` to protect features that fall below `threshold`
#' but should never be dissolved away.
#'
#' @param x an `sf` or `SpatVector` polygons object. A projected CRS is
#'   expected, so areas and border lengths are metric; warns otherwise.
#'
#' @param threshold sliver-area cutoff. Either a \pkg{units} object (e.g.
#'   `units::set_units(1, "ha")`) or a bare numeric in square metres. Polygons
#'   with area `<= threshold` are candidate slivers.
#'
#' @param keep optional <[`data-masking`][rlang::args_data_masking]> predicate
#'   evaluated against `x`'s attribute table; features for which it is `TRUE`
#'   are never treated as slivers, even when below `threshold`. Column names
#'   are used directly. Defaults to `NULL` (slivers by area alone). See
#'   examples.
#'
#' @param explode if `TRUE` (the default), multipart polygons are split to
#'   single part before sliver detection, so `threshold` applies to each
#'   individual ring. When `TRUE`, prefer a column-based `keep` predicate over
#'   an external vector, since exploding changes the number/order of features.
#'
#' @param boundary,overlap,distance,minover,maxdist,append passed through to
#'   [terra::combineGeoms()]. The defaults implement the *LENGTH* rule:
#'   `boundary = TRUE` assigns each sliver to the keeper it shares the longest
#'   border with; `overlap = FALSE` disables overlap-area precedence;
#'   `distance` (within `maxdist` map units) and `append = TRUE` only affect
#'   slivers that share no border with any keeper (the latter keeps them rather
#'   than dropping them). `combineGeoms()` always uses `dissolve`/`erase` TRUE.
#'
#' @returns an object of the same class as `x`, with slivers eliminated. If `x`
#'   has no slivers it is returned unchanged; if **every** feature is a sliver a
#'   warning is issued and `x` is returned unchanged (so the call is safe in a
#'   pipe).
#'
#' @aliases desliver
#' @seealso [terra::combineGeoms()], [intersect_clean()], [st_union_analysis()]
#'
#' @examples
#' library(sf)
#'
#' sq <- function(xmin, xmax, ymin, ymax, id) {
#'   p <- st_polygon(list(rbind(
#'     c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)
#'   )))
#'   st_sf(id = id, protected = id == "B", geometry = st_sfc(p, crs = 3005))
#' }
#' polys <- rbind(
#'   sq(0, 5, 0, 5, "A"), # 25; shares a length-5 border with the sliver
#'   sq(0, 1, 6, 16, "B"), # 10; shares a length-1 border with the sliver
#'   sq(0, 5, 5, 6, "S") #  5; the sliver
#' )
#'
#' ## merge polygons < 6 (map units^2) into their longest-border neighbour:
#' ## "S" is absorbed by "A" (border length 5 > 1); "B" is untouched.
#' eliminate_slivers(polys, threshold = 6)
#'
#' ## a units threshold works too (areas here are tiny, shown for syntax):
#' eliminate_slivers(polys, threshold = units::set_units(6, "m^2"))
#'
#' ## protect features via a data-masking predicate on the attribute table
#' ## ("S" is the only sliver here, but this is how you keep a small feature):
#' eliminate_slivers(polys, threshold = 6, keep = protected)
#' eliminate_slivers(polys, threshold = 6, keep = id == "S")
#' @export
eliminate_slivers <- function(
  x,
  threshold,
  keep = NULL,
  explode = TRUE,
  boundary = TRUE,
  overlap = FALSE,
  distance = TRUE,
  minover = 0.05,
  maxdist = 1,
  append = TRUE
) {
  was_sf <- inherits(x, "sf")
  v <- if (was_sf) terra::vect(x) else x
  if (!inherits(v, "SpatVector")) {
    stop("`x` must be an `sf` or `SpatVector` polygons object.")
  }
  if (terra::geomtype(v) != "polygons") {
    stop("`x` must contain polygon geometries, not ", terra::geomtype(v), ".")
  }
  if (isTRUE(terra::is.lonlat(v, warn = FALSE))) {
    warning(
      "`x` has a geographic CRS; sliver areas and border lengths are not metric. ",
      "Project to an equal-area / projected CRS before eliminating slivers."
    )
  }

  if (explode) {
    v <- terra::disagg(v)
  }

  ## sliver area cutoff in m^2 (terra::expanse returns m^2)
  thr_m2 <- if (inherits(threshold, "units")) {
    if (!requireNamespace("units", quietly = TRUE)) {
      stop("`threshold` is a units object but the 'units' package is not installed.")
    }
    as.numeric(units::set_units(threshold, "m^2", mode = "standard"))
  } else {
    as.numeric(threshold)
  }
  is_small <- terra::expanse(v, unit = "m") <= thr_m2

  ## features the caller wants to protect, via a data-masking predicate on the attributes
  keep_q <- rlang::enquo(keep)
  forced <- rep(FALSE, nrow(v))
  if (!rlang::quo_is_null(keep_q)) {
    forced <- as.logical(rlang::eval_tidy(keep_q, data = as.data.frame(v)))
    if (length(forced) != nrow(v)) {
      stop("`keep` must evaluate to one logical value per (exploded) feature.")
    }
    forced[is.na(forced)] <- FALSE
  }

  is_sliver <- is_small & !forced

  if (!any(is_sliver)) {
    return(x) # nothing to eliminate; return unchanged (and same type)
  }
  if (all(is_sliver)) {
    warning("All features are slivers (area <= threshold); returning `x` unchanged.")
    return(x)
  }

  merged <- terra::combineGeoms(
    v[!is_sliver, ],
    v[is_sliver, ],
    overlap = overlap,
    boundary = boundary,
    distance = distance,
    append = append,
    minover = minover,
    maxdist = maxdist,
    dissolve = TRUE,
    erase = TRUE
  )
  merged <- terra::makeValid(merged)

  if (!was_sf) {
    return(merged)
  }
  out <- sf::st_as_sf(merged)
  gcol <- attr(x, "sf_column") # preserve the caller's geometry-column name
  if (!is.null(gcol) && !identical(gcol, attr(out, "sf_column"))) {
    names(out)[names(out) == attr(out, "sf_column")] <- gcol
    out <- sf::st_set_geometry(out, gcol)
  }
  out
}

#' @rdname eliminate_slivers
#' @export
desliver <- eliminate_slivers
