#' Tag one polygon layer with another's attributes, as an overlay
#'
#' A left join done as an overlay: every part of `x` keeps its own attributes
#' and gains `y`'s wherever the two overlap, `NA` everywhere else. `x`'s
#' footprint is preserved exactly -- no area invented, none lost, and none
#' counted twice -- with geometries split at the overlay boundaries so that each
#' location carries exactly one set of values.
#'
#' @section Why not a spatial join:
#' [sf::st_join()] is not an overlay. It keeps whole geometries from `x` and
#' emits one copy per feature of `y` they touch, so a polygon spanning two of
#' `y`'s features comes back twice at full extent, each copy carrying a
#' different set of `y`'s values. Measured on one study area, joining a
#' disturbance layer to a forest inventory that way inflated 4.02 M polygons /
#' 4.09 Mha into 7.78 M polygons / 39.5 Mha.
#'
#' @section Why not a union:
#' `terra::union()` has the documented semantics of the ArcGIS `Union` tool,
#' which is what makes it the obvious choice, and on real geometry it returned
#' output polygons overlapping one another *and* a dissolved footprint smaller
#' than its input -- double-counting some land while dropping other land
#' outright, in opposite directions, which is what makes the totals look
#' plausibly rather than obviously wrong. Measured on a 2,933-polygon window it
#' gave 74,959.504 ha against a base of 74,738.411 ha with a dissolved footprint
#' of 74,656.115 ha: 303 ha double-counted, 82 ha lost, and 233 s against 8 s
#' here.
#'
#' Reported as <https://github.com/rspatial/terra/issues/2175> and fixed
#' upstream. Intersection and difference partition `x` exactly, so the result
#' here is a partition by construction rather than by trust.
#'
#' @param x `SpatVector` polygons; the left-hand layer, whose footprint is kept.
#' @param y `SpatVector` polygons whose attributes are carried onto `x`. May be
#'   empty or miss `x` entirely, in which case `y`'s columns come back all `NA`.
#'
#' @return `SpatVector` polygons covering exactly `x`, carrying the columns of
#'   `x` followed by those of `y` that `x` did not already have.
#'
#' @seealso [erase_polygons()], [intersect_relate()], [intersect_clean()]
#' @export
overlay_left_join <- function(x, y) {
  ## `y` legitimately misses `x` altogether. terra warns "no intersection" for
  ## that, which is expected and noisy; the empty result is handled below.
  inside <- withCallingHandlers(terra::intersect(x, y), warning = function(w) {
    if (grepl("no intersection", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })

  inside <- assert_synced(inside, "intersect()")
  outside <- assert_synced(erase_polygons(x, y), "erase_polygons()")

  ## The columns the result must end up with, taken from the *inputs* rather
  ## than from either overlay result: an empty overlay can come back with its
  ## attribute columns dropped, and rebuilding the table from that would
  ## silently discard them.
  want <- c(names(x), setdiff(names(y), names(x)))

  ## one row of each input, purely to carry the column types
  proto <- c(
    as.list(terra::values(x)[0, , drop = FALSE]),
    as.list(terra::values(y)[0, , drop = FALSE])
  )

  ## Columns `y` contributes are NA wherever `y` does not reach. Build each
  ## attribute table in full and set it in one go: assigning column by column
  ## with `[[<-` fails on some real inputs with "cannot add these values", and
  ## says nothing about which column or why.
  fill <- function(v) {
    if (nrow(v) == 0L) {
      return(v)
    }

    d <- terra::values(v)

    for (nm in want) {
      if (is.null(d[[nm]])) {
        d[[nm]] <- rep(proto[[nm]][NA_integer_], nrow(d))
      }
    }

    terra::values(v) <- d[, want, drop = FALSE]
    v
  }

  parts <- Filter(\(v) nrow(v) > 0L, list(fill(inside), fill(outside)))

  if (length(parts) == 0L) {
    empty <- x[integer(0), ]
    terra::values(empty) <- as.data.frame(
      lapply(proto[want], \(p) p[integer(0)]),
      stringsAsFactors = FALSE
    )

    return(empty)
  }

  ## `unname()` matters: `rbind` matches named list elements to its own formals
  do.call(rbind, unname(parts))
}
