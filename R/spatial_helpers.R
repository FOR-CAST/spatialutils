#' Save geospatial vector data to a GeoPackage
#'
#' Thin wrapper around [sf::st_write()] that writes an `sf` object to a
#' GeoPackage (`.gpkg`) at the supplied destination path, overwriting any
#' existing layer.
#'
#' @param obj An `sf` object to write.
#' @param dst Character. Full destination path (including file name and
#'   extension) to write to.
#'
#' @return Invisibly returns `dst`, the destination path written to.
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' pts <- sf::st_sf(
#'   id = 1:2,
#'   geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
#' )
#' dst <- tempfile(fileext = ".gpkg")
#' save_gpkg(pts, dst)
save_gpkg <- function(obj, dst) {
  sf::st_write(obj, dst, quiet = TRUE, append = FALSE, delete_dsn = TRUE)

  invisible(dst)
}

#' Create a bounding box of a study area
#'
#' Convenience wrapper around [sf::st_bbox()] used to derive a bounding box
#' from a study area, e.g. to reduce processing time of large vector datasets.
#'
#' @param studyArea An `sf` or `sfc` object (or anything accepted by
#'   [sf::st_bbox()]).
#'
#' @return An object of class `bbox` (see [sf::st_bbox()]).
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' poly <- sf::st_sfc(
#'   sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
#'   crs = 4326
#' )
#' create_bbox(poly)
create_bbox <- function(studyArea) {
  sf::st_bbox(studyArea)
}

#' Get the coordinates of the centroid of a polygon
#'
#' Computes the centroid of a polygon and returns its coordinates as a numeric
#' vector. The attribute-geometry relationship is set to `"constant"` to
#' suppress [sf::st_centroid()] warnings.
#'
#' @param poly An `sf` or `sfc` polygon object.
#'
#' @return A numeric vector of centroid coordinates (`x`, `y`).
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' poly <- sf::st_sf(
#'   id = 1,
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
#'     crs = 4326
#'   )
#' )
#' calc_centroid(poly)
calc_centroid <- function(poly) {
  sf::st_centroid(sf::st_set_agr(poly, "constant")) |> sf::st_coordinates() |> as.numeric()
}

#' Convert sf coordinates to numeric
#'
#' Extracts coordinates from an `sf` points object and returns them as a plain
#' numeric vector, dropping any geometry list-column.
#'
#' @param points_sf An `sf` or `sfc` points object.
#'
#' @return A numeric vector of coordinates.
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' pt <- sf::st_sf(
#'   id = 1,
#'   geometry = sf::st_sfc(sf::st_point(c(3, 4)), crs = 4326)
#' )
#' extract_coords(pt)
extract_coords <- function(points_sf) {
  sf::st_coordinates(points_sf) |> sf::st_drop_geometry() |> as.numeric()
}

#' Spatial join based on intersection
#'
#' Casts `x` to `POLYGON` geometries and subsets it to those features that
#' intersect `y`, using [sf::st_intersects()] as the join predicate. The
#' attribute-geometry relationship of `x` is set to `"constant"` to suppress
#' [sf::st_cast()] warnings.
#'
#' @param x An `sf` (multi)polygon object to subset.
#' @param y An `sf` object to intersect against.
#'
#' @return An `sf` object: the subset of (cast) features of `x` that intersect
#'   `y`.
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' grid <- sf::st_sf(
#'   id = 1:2,
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
#'     sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)))),
#'     crs = 4326
#'   )
#' )
#' pt <- sf::st_sf(
#'   geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), crs = 4326)
#' )
#' spatial_join_intersects(grid, pt)
spatial_join_intersects <- function(x, y) {
  sf::st_cast(sf::st_set_agr(x, "constant"), "POLYGON")[y, , op = sf::st_intersects]
}

#' Prepare an aggregated lon/lat raster for climate-grid retrieval
#'
#' Extends a template raster to a target extent, reprojects it to a geographic
#' (lon/lat, EPSG:4326) coordinate reference system, and aggregates it to an
#' approximate target resolution. This is used to prepare a coarse point grid
#' for climate-data retrieval (e.g. BioSIM, ERA5).
#'
#' @param ext A `SpatExtent` (or object coercible to one by [terra::extend()])
#'   giving the target extent.
#' @param rtm A `SpatRaster` template ("raster to match") supplying the source
#'   resolution and CRS.
#' @param agg_to Numeric. Target resolution (in the units of `rtm`) used to
#'   compute the aggregation factor relative to `terra::res(rtm)`. Defaults to
#'   `1000`.
#'
#' @return A `SpatRaster` extended, reprojected to EPSG:4326, and aggregated.
#'
#' @family spatial helpers
#' @export
#'
#' @examples
#' r <- terra::rast(
#'   xmin = 0, xmax = 100, ymin = 0, ymax = 100,
#'   resolution = 10, crs = "epsg:3005"
#' )
#' terra::values(r) <- seq_len(terra::ncell(r))
#' prep_agg_lonlat_raster(terra::ext(r), r, agg_to = 50)
prep_agg_lonlat_raster <- function(ext, rtm, agg_to = 1000) {
  agg_fact <- agg_to / unique(terra::res(rtm))

  terra::extend(rtm, ext) |> terra::project(terra::crs("epsg:4326")) |> terra::aggregate(agg_fact)
}
