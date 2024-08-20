#' @keywords internal
#' @importFrom sf st_bbox
createExtent <- function(x, offset = NULL) {
  if (is_extent(x)) {
    return(x)
  }

  rlang::check_installed("raster")

  if (is_raster(x)) {
    rlang::check_installed("sp")
    ext <- raster::extent(
      raster::projectExtent(x, crs = sp::CRS("+init=epsg:4326"))
    )
  } else if (is_spatial(x)) {
    ext <- raster::extent(
      raster::xmin(x),
      raster::xmax(x),
      raster::ymin(x),
      raster::ymax(x)
    )
  } else if (inherits(x, "sfc") || inherits(x, "sf") ||
    inherits(x, "XY") || inherits(x, "stars")) {
    bb <- sf::st_bbox(x)
    ext <- raster::extent(bb[1], bb[3], bb[2], bb[4])
  }

  if (is.null(offset)) {
    xxtend <- c(ext[1], ext[2])
    yxtend <- c(ext[3], ext[4])
    ext@xmin <- xxtend[1]
    ext@xmax <- xxtend[2]
    ext@ymin <- yxtend[1]
    ext@ymax <- yxtend[2]

    return(ext)
  }

  ext@xmin <- ext@xmin - offset
  ext@xmax <- ext@xmax + offset
  ext@ymin <- ext@ymin - offset
  ext@ymax <- ext@ymax + offset

  ext
}
