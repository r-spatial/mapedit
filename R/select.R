#' Interactively Select Map Features
#'
#' @param x map to use
#'
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
selectFeatures = function(x, ...) {
  UseMethod("selectFeatures")
}

#' @export
selectFeatures.sf = function(x, platform = c("mapview", "leaflet"), ...) {

  if (length(platform) > 1) platform = platform[1]

  x = mapview:::checkAdjustProjection(x)
  x$edit_id = as.character(1:nrow(x))

  if (platform == "mapview") {
    m = mapview::mapview()@map
    m = mapview::addFeatures(m, data=x, layerId=~x$edit_id)
    m = leaflet::fitBounds(m,
                           lng1 = as.numeric(sf::st_bbox(x)[1]),
                           lat1 = as.numeric(sf::st_bbox(x)[2]),
                           lng2 = as.numeric(sf::st_bbox(x)[3]),
                           lat2 = as.numeric(sf::st_bbox(x)[4]))
    m = mapview::addHomeButton(map = m, ext = mapview:::createExtent(x))
  } else {
    m = leaflet::addTiles(leaflet::leaflet())
    m = mapview::addFeatures(m, data=x, layerId=~x$edit_id)
  }

  ind = selectMap(m, ...)

  indx = ind$id[as.logical(ind$selected)]
  # todrop = "edit_group"
  return(x[as.numeric(indx), !names(x) %in% "edit_group"])
}

#' @export
selectFeatures.Spatial = function(x, ...) {
  selectFeatures(sf::st_as_sf(x), ...)
}
