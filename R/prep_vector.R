#' Crop, mask, and reproject a vector to a study area
#'
#' A lean, `terra`-based replacement for the crop/mask/reproject that
#' `reproducible::prepInputs()` performed internally (via `postProcessTo()`),
#' without the caching, options, and download machinery. Intended to run after a
#' layer has been fetched and read, e.g. with
#' `workflowtools::drive_download_once()` +
#' `workflowtools::archive_extract_once()` + [terra::vect()].
#'
#' The processing mirrors what `prepInputs()` does, in this order:
#'
#' 1. repair `x` (and `studyArea`) topology with [terra::makeValid()];
#' 2. **pre-crop in the source CRS** -- project the (small) `studyArea` into
#'    `x`'s CRS, take a buffered extent, and crop `x` to it, so large or complex
#'    layers are reduced *before* the costly reprojection (rather than
#'    reprojecting the whole source);
#' 3. reproject the reduced layer to `crs`;
#' 4. clip precisely to the `studyArea` polygon (when `mask = TRUE`).
#'
#' Geometry validity is repaired after **every** crop/mask/reproject step, since
#' each of those operations can introduce invalid geometries.
#'
#' @param x A `SpatVector`, or a file path or data source readable by
#'   [terra::vect()].
#' @param studyArea A `SpatVector` (or a source readable by [terra::vect()])
#'   defining the area of interest, to which `x` is clipped. May be in any CRS.
#' @param crs Target coordinate reference system, in any form accepted by
#'   [terra::project()]. Defaults to the CRS of `studyArea`.
#' @param mask Logical. If `TRUE` (the default), clip `x` to the `studyArea`
#'   polygon geometry; if `FALSE`, crop to its (buffered) extent only.
#' @param makeValid Logical. If `TRUE` (the default), repair geometry with
#'   [terra::makeValid()] before and after each geometry operation.
#' @param buffer Numeric. Fraction of the larger study-area extent dimension by
#'   which to expand the source-CRS pre-crop extent (default `0.1`); `0`
#'   disables the buffer. The pre-crop extent is always clamped to `x`'s own
#'   extent.
#'
#' @return A `SpatVector` reprojected to `crs` and clipped to `studyArea`,
#'   retaining only the attributes of `x`.
#'
#' @export
#' @examples
#' studyArea <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
#' terra::crs(studyArea) <- "EPSG:3857"
#' x <- terra::vect("POLYGON ((8 4, 12 4, 12 6, 8 6, 8 4))")
#' terra::crs(x) <- "EPSG:3857"
#' prep_vector(x, studyArea)
prep_vector <- function(
  x,
  studyArea,
  crs = terra::crs(studyArea),
  mask = TRUE,
  makeValid = TRUE,
  buffer = 0.1
) {
  if (!inherits(x, "SpatVector")) {
    x <- terra::vect(x)
  }
  if (!inherits(studyArea, "SpatVector")) {
    studyArea <- terra::vect(studyArea)
  }

  fix <- function(v) {
    if (isTRUE(makeValid) && nrow(v) > 0L) terra::makeValid(v) else v
  }

  x <- fix(x)
  studyArea <- fix(studyArea)

  ## 1. pre-crop in the SOURCE CRS: project the (small) study area into x's CRS,
  ##    take a buffered extent (clamped to x), and crop -- reduces large/complex
  ##    layers before the costly reprojection.
  saSource <- fix(terra::project(studyArea, x))
  es <- terra::ext(saSource)
  ex <- terra::ext(x)
  ## if the study-area extent does not overlap the layer at all there is no
  ## intersection -- return an empty SpatVector (callers test `nrow == 0`),
  ## rather than letting an empty extent reach terra::crop() (which errors).
  noOverlap <- terra::xmax(es) <= terra::xmin(ex) ||
    terra::xmin(es) >= terra::xmax(ex) ||
    terra::ymax(es) <= terra::ymin(ex) ||
    terra::ymin(es) >= terra::ymax(ex)
  if (noOverlap) {
    return(x[integer(0)])
  }
  e <- es
  if (buffer > 0) {
    span <- max(terra::xmax(es) - terra::xmin(es), terra::ymax(es) - terra::ymin(es))
    e <- terra::intersect(terra::extend(es, buffer * span), ex)
  }
  x <- fix(terra::crop(x, e))

  ## 2. reproject the reduced layer to the target CRS.
  x <- fix(terra::project(x, crs))

  ## 3. clip precisely to the study-area polygon, in the target CRS. Aggregating
  ##    the study area drops its attributes so the clip keeps only those of x.
  if (isTRUE(mask)) {
    saTarget <- fix(terra::project(terra::aggregate(studyArea), crs))
    x <- fix(terra::crop(x, saTarget))
  }
  x
}
