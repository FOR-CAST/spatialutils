#' Repair invalid geometries efficiently
#'
#' Validates a `SpatVector` once and passes **only** the invalid geometries to
#' [terra::makeValid()], then recombines them with the already-valid majority.
#' This avoids running `makeValid()` over an entire layer -- slow on large
#' records (e.g. national fire perimeters) where only a small fraction of
#' geometries are invalid. Any geometry that remains invalid or empty after
#' repair is dropped, so the result contains only valid, non-empty geometries.
#'
#' @param v A `SpatVector`, or an object coercible to one via [terra::vect()].
#'
#' @returns A `SpatVector` of valid, non-empty geometries, in the same CRS and
#'   carrying the same attributes; repaired features keep their rows.
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' bowtie <- terra::vect("POLYGON ((0 0, 2 2, 0 2, 2 0, 0 0))") # self-intersecting
#' terra::is.valid(bowtie)
#' terra::is.valid(repair_geoms(bowtie))
repair_geoms <- function(v) {
  if (!inherits(v, "SpatVector")) {
    v <- terra::vect(v)
  }
  valid <- terra::is.valid(v)
  if (!all(valid)) {
    repaired <- terra::makeValid(v[!valid, ])
    repaired <- repaired[terra::is.valid(repaired), ] ## drop any still-invalid after repair
    v <- rbind(v[valid, ], repaired)
  }
  v[!terra::is.empty(v), ]
}
