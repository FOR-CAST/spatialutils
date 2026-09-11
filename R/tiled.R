#' Apply a two-layer overlay tile by tile
#'
#' Crops both layers to each tile, applies `fun`, and row-binds the results. Internal engine behind
#' [erase_polygons_tiled()] and [sym_difference_tiled()].
#'
#' @section When tiling is valid:
#' Only for **local** overlays -- where a feature's result depends solely on geometry near it.
#' That holds for difference, symmetric difference and intersection. It does NOT hold for anything
#' needing global context: dissolving, nearest-neighbour, distance. Tiling those silently gives
#' wrong answers, because a feature's nearest neighbour may sit in another tile.
#'
#' @param x,y `SpatVector` polygon layers.
#' @param tiles `SpatVector` or `sf` of tile polygons that together cover `x`. Reuse an existing
#'   grid where one exists: a layer built on a grid already carries vertices lying exactly on those
#'   seam coordinates, so re-cutting intersects at existing vertices rather than inventing new ones,
#'   and the halves of a split feature abut without a gap.
#' @param fun function of `(x, y)` returning a `SpatVector`.
#'
#' @return `SpatVector`, the row-bound per-tile results. Geometries crossing a tile seam are SPLIT;
#'   dissolve after this returns, never per tile, if they need rejoining.
#'
#' @keywords internal
#' @noRd
overlay_tiled <- function(x, y, tiles, fun) {
  ## work in whatever class `x` is, so an sf caller is never pushed through terra and back
  use_sf <- inherits(x, "sf")

  if (use_sf) {
    if (!inherits(tiles, "sf")) {
      tiles <- sf::st_as_sf(tiles)
    }
    clip <- function(v, aoi) suppressWarnings(sf::st_intersection(v, sf::st_geometry(aoi)))
  } else {
    if (!inherits(tiles, "SpatVector")) {
      tiles <- terra::vect(tiles)
    }
    clip <- function(v, aoi) terra::crop(v, aoi)
  }

  parts <- lapply(seq_len(nrow(tiles)), function(i) {
    aoi <- tiles[i, ]

    xi <- clip(x, aoi)
    if (nrow(xi) == 0L) {
      return(NULL)
    }

    yi <- clip(y, aoi)

    out <- fun(xi, yi)
    if (nrow(out) == 0L) NULL else out
  })

  parts <- Filter(Negate(is.null), parts)

  if (length(parts) == 0L) {
    return(x[integer(0), ])
  }

  do.call(rbind, parts)
}
