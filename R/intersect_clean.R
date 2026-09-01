#' Intersect two polygon layers and absorb the resulting slivers
#'
#' Useful where two sets of polygons do not align perfectly: intersecting them
#' leaves slivers along every mismatched boundary
#' (<https://github.com/r-spatial/sf/issues/547>), carrying attribute
#' combinations that are artefacts of the misalignment rather than real.
#'
#' Each sliver is **merged into the neighbouring polygon it shares the longest
#' border with** (see [eliminate_slivers()]), so no area is lost and the sliver
#' takes the attributes of a real neighbour.
#'
#' Slivers are judged, and absorbed, within each feature set separately:
#'
#' 1. identify the feature sets in `x` from the unique values of `x[[xcol]]`;
#' 2. for each set, take the area of its smallest feature in `x`;
#' 3. intersect with `y` and repair the result;
#' 4. within each set, treat any fragment smaller than
#'    `areaThresh * min(area)` as a sliver and merge it into its
#'    longest-shared-border neighbour **in that same set**.
#'
#' Keeping the merge within a set is what repairs the misalignment: a sliver of
#' set `A` carrying the wrong `y` attributes is absorbed by the main body of
#' `A`, which carries the right ones.
#'
#' @section Changed behaviour:
#' This previously *discarded* slivers via `smoothr::drop_crumbs()`, so the
#' result no longer covered the same footprint as the intersection and the area
#' in the dropped fragments simply vanished. Merging is the geometrically
#' sensible treatment and is what the ArcGIS `Eliminate` tool does.
#'
#' @param x,y `sf` polygons object
#' @param xcol character, name of the attribute column in `x` to use to identify features
#' @param areaThresh numeric, *proportion* of minimum polygon area to use as threshold sliver size.
#'
#' @return `sf` polygons object covering the same area as `st_intersection(x, y)`
#'
#' @seealso [eliminate_slivers()], [intersect_relate()]
#' @export
intersect_clean <- function(x, y, xcol, areaThresh = 0.05) {
  x <- sf::st_set_agr(x, "constant")
  y <- sf::st_set_agr(y, "constant")

  ## `keep_polygons()` rather than a GEOMETRYCOLLECTION test on its own: two polygons that share
  ## only an edge intersect in a bare LINESTRING, with no collection anywhere in the result, and
  ## `st_cast()` below then fails with "`x` must contain polygon geometries, not lines".
  xy <- sf::st_intersection(x, y) |> sf::st_make_valid() |> keep_polygons()

  ## only reshape when there is something to reshape -- `st_cast()` warns when asked to do nothing
  if (any(sf::st_geometry_type(xy) == "MULTIPOLYGON")) {
    xy <- sf::st_cast(xy, "POLYGON", warn = FALSE)
  }

  ## An intersection can come back with nothing in it: the layers only touch along an edge, or do
  ## not meet at all. There is no set to judge slivers against, and `do.call(rbind, list())` is
  ## NULL, which fails two lines further on rather than here.
  if (nrow(xy) == 0L) {
    return(xy)
  }

  names.x <- unique(xy[[xcol]])

  ## sliver cutoff per feature set: a proportion of the smallest feature of that set in `x`
  thresholds <- vapply(
    names.x,
    function(p) min(as.numeric(sf::st_area(x[x[[xcol]] == p, ]))) * areaThresh,
    numeric(1)
  )

  z <- lapply(names.x, function(p) {
    polys <- xy[xy[[xcol]] == p, ]

    ## `eliminate_slivers()` takes a single threshold and its own `keep` predicate, so flag the
    ## slivers here (against this set's threshold) and protect everything else.
    polys[[".sliver"]] <- as.numeric(sf::st_area(polys)) <= thresholds[[p]]

    merged <- eliminate_slivers(
      polys,
      threshold = Inf, ## every feature is a candidate; `keep` decides
      keep = !.data$.sliver,
      explode = FALSE ## already single-part
    )

    merged[[".sliver"]] <- NULL

    merged
  })

  z <- do.call(rbind, z)

  z[!sf::st_is_empty(z), ]
}
