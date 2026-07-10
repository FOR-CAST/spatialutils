#' Read a vector layer, filtered to an area of interest
#'
#' Read a (potentially very large) vector source while pushing the spatial
#' filter down to the GDAL/OGR read, so that only geometries overlapping
#' `aoi` are materialised in R. This is the memory-efficient way to work with
#' enormous polygon datasets (e.g. provincial landbase coverages): the bulk of
#' the features never enter R.
#'
#' The read proceeds in three steps:
#'
#' 1. the source CRS is obtained without reading any geometries (via a
#'    [terra::vect()] *proxy*), and `aoi` is projected into it;
#' 2. the layer is read via [terra::query()] on the proxy, pushing **both** the
#'    column selection (`vars = fields`) and a spatial `filter` (the projected
#'    `aoi`) down to the GDAL/OGR read -- so wide attribute tables (a landbase
#'    coverage can have >100 columns) are never materialised in R. GDAL
#'    guarantees all extent-overlapping features are returned but *may* return
#'    extras;
#' 3. the result is refined with [terra::relate()] so exactly the features
#'    satisfying `relation` are kept.
#'
#' If the driver cannot honour the pushed-down read it falls back to a spatial
#' `filter`-only read with the columns subset in R; the `relate()` refinement
#' (and a final column subset) run regardless, so the result is correct even
#' when a driver silently ignores the pushed-down filter.
#'
#' @param src A `SpatVector`, or a file path / data source readable by
#'   [terra::vect()] (e.g. a shapefile, GeoPackage, or File Geodatabase).
#' @param aoi A `SpatVector` (or a source readable by [terra::vect()]) defining
#'   the area of interest. May be in any CRS.
#' @param fields Optional character vector of attribute columns to keep. Columns
#'   not present in the source are ignored; `NULL` (the default) keeps all.
#' @param layer Optional layer name, for sources (e.g. a File Geodatabase) with
#'   more than one layer.
#' @param relation Character. The spatial predicate passed to
#'   [terra::relate()] used to refine the GDAL filter (default
#'   `"intersects"`).
#'
#' @return A `SpatVector` in the source CRS, containing only the features of
#'   `src` related to `aoi` and only the requested `fields`. Callers should test
#'   `nrow() == 0L` for the no-overlap case.
#'
#' @export
#' @examples
#' aoi <- terra::vect("POLYGON ((0 0, 5 0, 5 5, 0 5, 0 0))")
#' terra::crs(aoi) <- "EPSG:3857"
#' near <- terra::vect("POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))")
#' far <- terra::vect("POLYGON ((20 20, 21 20, 21 21, 20 21, 20 20))")
#' src <- rbind(near, far)
#' terra::crs(src) <- "EPSG:3857"
#' read_vector_aoi(src, aoi)
read_vector_aoi <- function(src, aoi, fields = NULL, layer = NULL, relation = "intersects") {
  if (!inherits(aoi, "SpatVector")) {
    aoi <- terra::vect(aoi)
  }
  aoi1 <- terra::aggregate(aoi)

  if (inherits(src, "SpatVector")) {
    v <- src
    aoiSrc <- terra::project(aoi1, v)
  } else {
    lyr <- if (is.null(layer)) "" else layer
    proxy <- terra::vect(src, layer = lyr, proxy = TRUE)
    aoiSrc <- terra::project(aoi1, terra::crs(proxy))

    ## Push BOTH the column selection and the spatial filter down to the read.
    ## Some drivers (e.g. GeoPackage) drop the geometry field when specific
    ## columns are selected, which disables the spatial filter (a GDAL warning);
    ## detect that and fall back to a spatial-filter-only read (geometry always
    ## present), subsetting the columns in R afterwards.
    pushOK <- TRUE
    v <- withCallingHandlers(
      tryCatch(
        if (is.null(fields)) {
          terra::query(proxy, filter = aoiSrc)
        } else {
          terra::query(proxy, vars = fields, filter = aoiSrc)
        },
        error = function(e) {
          pushOK <<- FALSE
          NULL
        }
      ),
      warning = function(w) {
        if (grepl("geometry field|spatial filter", conditionMessage(w), ignore.case = TRUE)) {
          pushOK <<- FALSE
          invokeRestart("muffleWarning")
        }
      }
    )
    if (!isTRUE(pushOK) || is.null(v)) {
      v <- terra::vect(src, layer = lyr, filter = aoiSrc)
    }
  }

  ## refine the (coarse, extent-based) filter to the exact spatial predicate
  if (nrow(v) > 0L) {
    v <- v[terra::is.related(v, aoiSrc, relation), ]
  }

  ## guarantee only the requested columns (idempotent after the fallback path)
  if (!is.null(fields) && nrow(v) > 0L) {
    keep <- intersect(fields, names(v))
    if (length(keep) > 0L) {
      v <- v[, keep]
    }
  }

  v
}

#' Intersect two vectors, skipping non-intersecting features
#'
#' A `terra`-native intersection that first drops the features of each input
#' that cannot possibly intersect the other (via [terra::relate()]), so the
#' underlying GEOS overlay only processes candidate geometries. Equivalent in
#' result to [terra::intersect()], but avoids feeding it large, mostly-disjoint
#' inputs.
#'
#' @param x,y `SpatVector`s (or sources readable by [terra::vect()]).
#'
#' @return A `SpatVector` of the intersection of the intersecting subsets of
#'   `x` and `y`; an empty `SpatVector` when they are disjoint.
#'
#' @export
#' @examples
#' x <- terra::vect("POLYGON ((0 0, 3 0, 3 3, 0 3, 0 0))")
#' y <- terra::vect("POLYGON ((2 2, 5 2, 5 5, 2 5, 2 2))")
#' intersect_relate(x, y)
intersect_relate <- function(x, y) {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }
  if (!inherits(y, "SpatVector")) {
    y <- terra::vect(y)
  }

  xr <- x[terra::is.related(x, y, "intersects"), ]
  yr <- y[terra::is.related(y, x, "intersects"), ]
  if (nrow(xr) == 0L || nrow(yr) == 0L) {
    return(xr[integer(0)])
  }

  terra::intersect(xr, yr)
}

#' Prepare a landbase-status polygon layer
#'
#' Port of the per-FMA "active/passive" (a.k.a. contributing/non-contributing)
#' landbase-status processing formerly repeated across the `LandWeb_preamble`
#' study-area helpers. Reads a landbase coverage, keeps only the polygons within
#' `aoi` and only the status column, repairs and dissolves them by status, and
#' clips the result to `aoi`.
#'
#' The read is spatially and column-filtered up front (see [read_vector_aoi()]),
#' so an enormous source coverage is reduced to the AOI before any geometry
#' repair or dissolve, and only the handful of dissolved polygons are reprojected
#' and clipped.
#'
#' Two dissolve methods are offered because an exact vector union of a large
#' landbase (hundreds of thousands of overlay polygons) is very slow -- minutes
#' to hours for the whole thing:
#'
#' * `"raster"` (the default) rasterises the features by status at `dissolve_res`
#'   and polygonises the result -- orders of magnitude faster, with boundaries
#'   snapped to the `dissolve_res` grid. For reporting polygons (which are
#'   rasterised to the simulation grid downstream anyway) this is immaterial as
#'   long as `dissolve_res` is finer than the analysis grid.
#' * `"vector"` uses an exact [terra::aggregate()] union (v2-equivalent
#'   boundaries), at a large time cost.
#'
#' @inheritParams read_vector_aoi
#' @param status_col Character. Name of the source attribute holding the
#'   landbase status (e.g. `"f_active"`, `"LBC_LBStatus"`, `"F_CONDITIO"`,
#'   `"LBC_Landbase"`).
#' @param crs Target coordinate reference system, in any form accepted by
#'   [terra::project()]. Defaults to the CRS of `aoi`.
#' @param status_out Character. Name to give the standardised status column in
#'   the output (default `"lbstatus"`).
#' @param dissolve How to dissolve features by status: `"raster"` (the default,
#'   fast, grid-snapped), `"vector"` (exact but slow), or `"none"` (keep
#'   individual features).
#' @param dissolve_res Numeric. Grid resolution (in the units of the source CRS)
#'   for `dissolve = "raster"` (default `30`).
#' @param mask Logical. If `TRUE` (the default), clip the result precisely to
#'   the `aoi` polygon; if `FALSE`, keep whole (intersecting) features.
#'
#' @return A `SpatVector` in `crs`, with a single `status_out` column, dissolved
#'   by status (unless `dissolve = "none"`) and clipped to `aoi` (when
#'   `mask = TRUE`). Empty (`nrow == 0`) when no landbase features fall in `aoi`.
#'
#' @seealso [read_vector_aoi()], [intersect_relate()]
#' @export
#' @examples
#' aoi <- terra::vect("POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0))")
#' terra::crs(aoi) <- "EPSG:3857"
#' a1 <- terra::vect("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))")
#' a2 <- terra::vect("POLYGON ((2 0, 4 0, 4 2, 2 2, 2 0))")
#' p1 <- terra::vect("POLYGON ((0 2, 4 2, 4 4, 0 4, 0 2))")
#' src <- rbind(a1, a2, p1)
#' src$f_active <- c("Active", "Active", "Passive")
#' terra::crs(src) <- "EPSG:3857"
#' prep_landbase(src, aoi, status_col = "f_active")
prep_landbase <- function(
  src,
  aoi,
  status_col,
  layer = NULL,
  crs = NULL,
  status_out = "lbstatus",
  dissolve = c("raster", "vector", "none"),
  dissolve_res = 30,
  mask = TRUE
) {
  dissolve <- match.arg(dissolve)
  if (!inherits(aoi, "SpatVector")) {
    aoi <- terra::vect(aoi)
  }
  if (is.null(crs)) {
    crs <- terra::crs(aoi)
  }

  v <- read_vector_aoi(src, aoi, fields = status_col, layer = layer)
  if (!status_col %in% names(v)) {
    stop(
      "`status_col` \"", status_col, "\" not found in the source layer; ",
      "available columns: ", paste(names(v), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (nrow(v) == 0L) {
    return(v)
  }

  ## repair, then drop any geometries that remain invalid or empty
  v <- terra::makeValid(v)
  v <- v[terra::is.valid(v), ]
  v <- v[!terra::is.empty(v), ]

  ## standardise the status column name
  names(v)[names(v) == status_col] <- status_out

  ## dissolve by status in the source CRS (reduce many -> few, before reprojecting)
  v <- switch(
    dissolve,
    raster = .dissolveRaster(v, status_out, dissolve_res),
    vector = terra::aggregate(v, by = status_out)[, status_out], ## drop agg_n
    none = v
  )

  ## reproject the reduced result, then clip precisely to the AOI
  v <- terra::makeValid(terra::project(v, crs))
  if (isTRUE(mask)) {
    aoiTarget <- terra::project(terra::aggregate(aoi), crs)
    v <- terra::makeValid(terra::crop(v, aoiTarget))
  }

  v
}

## Dissolve `v` by the `field` status column by rasterising at resolution `res`
## (source-CRS units) and polygonising -- far faster than an exact vector union
## for large, overlapping landbase coverages. Boundaries snap to the `res` grid.
.dissolveRaster <- function(v, field, res) {
  lev <- sort(unique(v[[field]][[1]]))
  v$..sid <- as.integer(factor(v[[field]][[1]], levels = lev))
  r <- terra::rasterize(v, terra::rast(v, resolution = res), field = "..sid")
  d <- terra::as.polygons(r, dissolve = TRUE)
  d[[field]] <- lev[d[[names(d)[1]]][[1]]]
  d[, field]
}
