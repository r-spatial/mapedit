#' @keywords internal
createExtent <- function(x, offset = NULL) {

  if (inherits(x, "Extent")) {
    return(x)
  } else {
    if (inherits(x, "Raster")) {
      ext <- raster::extent(
        raster::projectExtent(x, crs = sp::CRS("+init=epsg:4326")))
    } else if (inherits(x, "Spatial")) {
      ext <- raster::extent(raster::xmin(x),
                            raster::xmax(x),
                            raster::ymin(x),
                            raster::ymax(x))
    } else if (inherits(x, "sfc") | inherits(x, "sf") |
               inherits(x, "XY") | inherits(x, "stars")) {
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
    } else {
      ext@xmin <- ext@xmin - offset
      ext@xmax <- ext@xmax + offset
      ext@ymin <- ext@ymin - offset
      ext@ymax <- ext@ymax + offset
    }

    return(ext)
  }

}
