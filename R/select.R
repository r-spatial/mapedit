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
  x$edit_group = as.character(1:nrow(x))

  addfun = switch(as.character(sf::st_dimension(sf::st_geometry(x)))[1],
                  "0" = leaflet::addCircleMarkers,
                  "1" = leaflet::addPolylines,
                  "2" = leaflet::addPolygons)

  if (platform == "mapview") {
    m = mapview::mapview()@map
    m = addfun(map = m, data = x, weight = 1, group = ~edit_group)
    m = leaflet::fitBounds(m,
                           lng1 = as.numeric(sf::st_bbox(x)[1]),
                           lat1 = as.numeric(sf::st_bbox(x)[2]),
                           lng2 = as.numeric(sf::st_bbox(x)[3]),
                           lat2 = as.numeric(sf::st_bbox(x)[4]))
    m = mapview::addHomeButton(map = m, ext = mapview:::createExtent(x))
  } else {
    m = leaflet::addTiles(leaflet::leaflet())
    m = addfun(map = m, data = x, weight = 1, group = ~edit_group)
  }

  ind = selectMap(m, ...)

  indx = ind$group[as.logical(ind$selected)]
  # todrop = "edit_group"
  return(x[as.numeric(indx), !names(x) %in% "edit_group"])
}

#' @export
selectFeatures.Spatial = function(x, ...) {
  selectFeatures(sf::st_as_sf(x), ...)
}
