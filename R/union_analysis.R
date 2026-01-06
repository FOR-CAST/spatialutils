#' Emulate arcpy `Union_analysis`
#'
#' @param x,y sf polygon objects
#'
#' @param union_by optional character string (length 1) specifying the column to union by
#'
#' @note `sf::st_union` modifies geometries and does retain attributes,
#' and for large datasets, avoiding Cartesian joins is essential to minimize RAM use.
#'
#' @examples
#' nc <- system.file("shape/nc.shp", package = "sf") |>
#'   sf::st_read(quiet = TRUE) |>
#'   dplyr::select(NAME) |>
#'   sf::st_transform("+proj=laea +lat_0=30 +lon_0=-95")
#'
#' x <- nc[sample(1:nrow(nc), 40), ] |>
#'   dplyr::mutate(seral = "old", .before = "geometry") |>
#'   dplyr::group_by(seral) |>
#'   dplyr::summarise() |>
#'   sf::st_cast("POLYGON", warn = FALSE) |>
#'   sf::st_set_agr("constant")
#'
#' y <- nc[sample(1:nrow(nc), 30), ] |>
#'   dplyr::mutate(seral = "mature", .before = "geometry") |>
#'   dplyr::group_by(seral) |>
#'   dplyr::summarise() |>
#'   sf::st_buffer(-5000) |>
#'   sf::st_cast("POLYGON", warn = FALSE) |>
#'   sf::st_set_agr("constant")
#'
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = x, fill = "orange", alpha = 0.5) +
#'   ggplot2::geom_sf(data = y, fill = "darkgreen", alpha = 0.5)
#'
#' ## uses attribute values from x for intersections
#' i <- sf::st_intersection(x, y) |>
#'   sf::st_make_valid() |>
#'   dplyr::select(-dplyr::ends_with(".1"))
#'
#' dx <- sf::st_difference(x, sf::st_union(y))
#' dy <- sf::st_difference(y, sf::st_union(x))
#'
#' u <- rbind(i, dx, dy) |>
#'   dplyr::summarise()
#' identical(u, st_union_analysis(x, y))
#'
#' ggplot2::ggplot() +
#'   ggplot2::geom_sf(data = dx, fill = "lightblue", alpha = 0.25) +
#'   ggplot2::geom_sf(data = dy, fill = "darkred", alpha = 0.25) +
#'   ggplot2::geom_sf(data = i, fill = "purple", alpha = 0.25) +
#'   ggplot2::geom_sf(data = u, fill = NA, col = "black", alpha = 0.25, linewidth = 1.2)
#'
#' @export
st_union_analysis <- function(x, y, union_by = NULL) {
  x <- sf::st_set_geometry(x, "geometry")
  y <- sf::st_set_geometry(y, "geometry")

  i <- sf::st_intersection(x, y) |> sf::st_make_valid() |> dplyr::select(-dplyr::ends_with(".1"))

  dx <- sf::st_difference(x, sf::st_union(y))
  dy <- sf::st_difference(y, sf::st_union(x))

  names_diff_x <- setdiff(names(x), names(y))
  names_diff_y <- setdiff(names(y), names(x))

  if (length(names_diff_y) > 0) {
    dx <- dplyr::mutate(dx, {{ names_diff_y }} := NA_character_, .before = "geometry")
  }

  if (length(names_diff_x) > 0) {
    dy <- dplyr::mutate(dy, {{ names_diff_x }} := NA_character_, .before = "geometry")
  }

  u <- rbind(i, dx, dy)

  if (is.null(union_by)) {
    u <- u |> dplyr::summarise()
  } else {
    stopifnot(is.character(union_by), length(union_by) == 1)

    u <- u |> dplyr::group_by(.data[[union_by]]) |> dplyr::summarise()
  }

  return(u)
}
