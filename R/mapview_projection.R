### these are all copies of unexported functions in mapview

# Scale coordinates for unprojected spatial objects -----------------------

scaleCoordinates <- function(x.coords, y.coords) {

  if (length(x.coords) == 1) {
    x_sc <- y_sc <- 0
  } else {
    ratio <- diff(range(y.coords)) / diff(range(x.coords))
    x_sc <- scales::rescale(x.coords, to = c(0, 1))
    y_sc <- scales::rescale(y.coords, to = c(0, 1)) * ratio
  }
  return(cbind(x_sc, y_sc))

}



# Scale extent ------------------------------------------------------------

scaleExtent <- function(x) {
  ratio <- raster::nrow(x) / raster::ncol(x)
  x_sc <- scales::rescale(c(x@extent@xmin, x@extent@xmax), c(0, 1))
  y_sc <- scales::rescale(c(x@extent@ymin, x@extent@ymax), c(0, 1)) * ratio

  return(raster::extent(c(x_sc, y_sc)))
}


# Scale unprojected SpatialPolygons* objects ------------------------------

scalePolygonsCoordinates <- function(x) {

  coord_lst <- lapply(methods::slot(x, "polygons"), function(x) {
    lapply(methods::slot(x, "Polygons"), function(y) methods::slot(y, "coords"))
  })

  xcoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 1]
    })
  })))

  ycoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 2]
    })
  })))

  ratio <- diff(range(ycoords)) / diff(range(xcoords))

  x_mn <- min(xcoords, na.rm = TRUE)
  x_mx <- max(xcoords - min(xcoords, na.rm = TRUE), na.rm = TRUE)

  y_mn <- min(ycoords, na.rm = TRUE)
  y_mx <- max(ycoords - min(ycoords, na.rm = TRUE), na.rm = TRUE)

  #do.call("rbind",
  pols <- lapply(seq(coord_lst), function(j) {

    ## extract current 'Polygons'
    pys <- x@polygons[[j]]

    lst <- lapply(seq(pys@Polygons), function(h) {

      # extract current 'Polygon'
      py <- pys@Polygons[[h]]

      # rescale coordinates
      crd <- sp::coordinates(py)
      coords_rscl <- cbind((crd[, 1] - x_mn) / x_mx,
                           (crd[, 2] - y_mn) / y_mx * ratio)

      # assign new coordinates and label point
      methods::slot(py, "coords") <- coords_rscl
      methods::slot(py, "labpt") <- range(coords_rscl)

      return(py)
    })

    sp::Polygons(lst, ID = pys@ID)
    # sp::SpatialPolygons(list(sp::Polygons(lst, ID = pys@ID)),
    #                     proj4string = sp::CRS(sp::proj4string(x)))
  })#)

  x@polygons <- pols

  x_rng <- range(sapply(pols, function(i) sp::bbox(i)[1, ]))
  y_rng <- range(sapply(pols, function(i) sp::bbox(i)[2, ]))
  x@bbox <- matrix(c(x_rng[1], x_rng[2], y_rng[1], y_rng[2]),
                   ncol = 2, byrow = TRUE)
  return(x)
}


# Scale unprojected SpatialLines* objects ------------------------------

scaleLinesCoordinates <- function(x) {

  coord_lst <- lapply(methods::slot(x, "lines"), function(x) {
    lapply(methods::slot(x, "Lines"), function(y) methods::slot(y, "coords"))
  })

  xcoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 1]
    })
  })))

  ycoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 2]
    })
  })))

  ratio <- diff(range(ycoords)) / diff(range(xcoords))

  x_mn <- min(xcoords, na.rm = TRUE)
  x_mx <- max(xcoords - min(xcoords, na.rm = TRUE), na.rm = TRUE)

  y_mn <- min(ycoords, na.rm = TRUE)
  y_mx <- max(ycoords - min(ycoords, na.rm = TRUE), na.rm = TRUE)

  #do.call("rbind",
  lins <- lapply(seq(coord_lst), function(j) {

    ## extract current 'Lines'
    lns <- x@lines[[j]]

    lst <- lapply(seq(lns@Lines), function(h) {

      # extract current 'Line'
      ln <- lns@Lines[[h]]

      # rescale coordinates
      crd <- sp::coordinates(ln)
      coords_rscl <- cbind((crd[, 1] - x_mn) / x_mx,
                           (crd[, 2] - y_mn) / y_mx * ratio)

      # assign new coordinates and label point
      methods::slot(ln, "coords") <- coords_rscl

      return(ln)
    })

    sp::Lines(lst, ID = lns@ID)

    # sp::SpatialLines(list(sp::Lines(lst, ID = lns@ID)),
    #                     proj4string = sp::CRS(sp::proj4string(x)))
  })#)

  x@lines <- lins

  x_rng <- range(sapply(lins, function(i) sp::bbox(i)[1, ]))
  y_rng <- range(sapply(lins, function(i) sp::bbox(i)[2, ]))
  x@bbox <- matrix(c(x_rng[1], x_rng[2], y_rng[1], y_rng[2]),
                   ncol = 2, byrow = TRUE)
  return(x)
}


## the two crs we use
wmcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
llcrs <- "+proj=longlat +datum=WGS84 +no_defs"

non_proj_warning <-
  "supplied layer has no projection information and is shown without background map"

wrong_proj_warning <-
  paste0("projection of supplied layer is not leaflet conform.", "\n",
         "  projecting to '", llcrs, "'")

# Check and potentially adjust projection of objects to be rendered =======
#' @keywords internal
checkAdjustProjection <- function(x, method = "bilinear") {

  x <- switch(class(x)[1],
              "RasterLayer" = rasterCheckAdjustProjection(x, method),
              "RasterStack" = rasterCheckAdjustProjection(x, method),
              "RasterBrick" = rasterCheckAdjustProjection(x, method),
              "SpatialPointsDataFrame" = spCheckAdjustProjection(x),
              "SpatialPolygonsDataFrame" = spCheckAdjustProjection(x),
              "SpatialLinesDataFrame" = spCheckAdjustProjection(x),
              "SpatialPoints" = spCheckAdjustProjection(x),
              "SpatialPolygons" = spCheckAdjustProjection(x),
              "SpatialLines" = spCheckAdjustProjection(x),
              "sf" = sfCheckAdjustProjection(x),
              "XY" = sfCheckAdjustProjection(x),
              "sfc_POINT" = sfCheckAdjustProjection(x),
              "sfc_MULTIPOINT" = sfCheckAdjustProjection(x),
              "sfc_LINESTRING" = sfCheckAdjustProjection(x),
              "sfc_MULTILINESTRING" = sfCheckAdjustProjection(x),
              "sfc_POLYGON" = sfCheckAdjustProjection(x),
              "sfc_MULTIPOLYGON" = sfCheckAdjustProjection(x),
              "sfc_GEOMETRY" = sfCheckAdjustProjection(x),
              "sfc_GEOMETRYCOLLECTION" = sfCheckAdjustProjection(x))

  return(x)

}
#
#   if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
#     x <- rasterCheckAdjustProjection(x)
#   } else if (class(x)[1] %in% c("SpatialPointsDataFrame",
#                                 "SpatialPolygonsDataFrame",
#                                 "SpatialLinesDataFrame",
#                                 "SpatialPoints",
#                                 "SpatialPolygons",
#                                 "SpatialLines")) {
#     x <- spCheckAdjustProjection(x)
#   }
#
#   return(x)
# }


# Project Raster* objects for mapView =====================================
#' @keywords internal
rasterCheckAdjustProjection <- function(x, method) {

  is.fact <- raster::is.factor(x)[1]

  if (is.na(raster::projection(x))) {
    warning(non_proj_warning)
    raster::extent(x) <- scaleExtent(x)
    raster::projection(x) <- llcrs
  } else if (is.fact) {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = method)
  }

  return(x)

}


# Project stars* objects for mapView =====================================
#' @keywords internal
starsCheckAdjustProjection <- function(x, method) {

  # is.fact <- raster::is.factor(x)[1]

  # if (is.na(raster::projection(x))) {
  #   warning(non_proj_warning)
  #   raster::extent(x) <- scaleExtent(x)
  #   raster::projection(x) <- llcrs
  # } else if (is.fact) {
  #   x <- raster::projectRaster(
  #     x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
  #     method = "ngb")
  #   x <- raster::as.factor(x)
  # } else {
  x <- sf::st_transform(
    x,
    crs = llcrs
  )
  # }

  return(x)

}


# Check and potentially adjust projection of sf objects ===================
#' @keywords internal
sfCheckAdjustProjection <- function(x) {

  if (is.na(sf::st_crs(x))) {
    return(x) # warning(non_proj_warning)
  } else { #if (!validLongLat(sf::st_crs(x)$proj4string)) {
    x <- sf::st_transform(x, llcrs)
  }

  return(x)

}


# Check and potentially adjust projection of Spatial* objects =============
#' @keywords internal
spCheckAdjustProjection <- function(x) {

  if (is.na(raster::projection(x))) {
    warning(non_proj_warning)
    if (class(x)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints")) {
      methods::slot(x, "coords") <- scaleCoordinates(sp::coordinates(x)[, 1],
                                                     sp::coordinates(x)[, 2])
    } else if (class(x)[1] %in% c("SpatialPolygonsDataFrame",
                                  "SpatialPolygons")) {
      x <- scalePolygonsCoordinates(x)
    } else if (class(x)[1] %in% c("SpatialLinesDataFrame",
                                  "SpatialLines")) {
      x <- scaleLinesCoordinates(x)
    }

    raster::projection(x) <- llcrs

  } else if (!identical(raster::projection(x), llcrs)) {
    x <- sp::spTransform(x, CRSobj = llcrs)
  }

  return(x)

}

# Check projection of objects according to their keywords =================
# validLongLat <- function (p4s) {
#   proj <- datum <- nodefs <- FALSE
#   allWGS84 <- c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84",
#                 "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0")
#
#   p4s_splt = strsplit(p4s, " ")[[1]]
#
#   for (comp in allWGS84) {
#     if (comp %in% p4s_splt) {
#       if (comp == "+init=epsg:4326") {
#         proj <- datum <- nodefs <- TRUE
#       }
#       if (comp == "+proj=longlat") {
#         proj <- TRUE
#       }
#       if (comp == "+no_defs") {
#         nodefs <- TRUE
#       }
#       if (comp == "+datum=WGS84") {
#         datum <- TRUE
#       }
#     }
#   }
#   if (proj & datum & nodefs) {
#     return(TRUE)
#   } else {
#     warning(wrong_proj_warning)
#     return(FALSE)
#   }
# }

