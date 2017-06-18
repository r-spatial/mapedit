#' Interactively Select Map Features
#'
#' @param x features to select
#' @param viewer \code{function} for the viewer.  See \code{\link[shiny]{viewer}}.
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
selectFeatures = function(x, viewer, ...) {
  UseMethod("selectFeatures")
}


#' @name selectFeatures
#' @param platform one of \code{"leaflet"} or \code{"mapview"} to indicate
#'          the type of map to use for selection
#' @param index \code{logical} with \code{index=TRUE} indicating return
#'          the index of selected features rather than the actual
#'          selected features
#' @export
selectFeatures.sf = function(
  x = NULL,
  platform = c("mapview", "leaflet"),
  index = FALSE,
  viewer = shiny::paneViewer(),
  ...
) {

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

  ind = selectMap(m, viewer=viewer, ...)

  indx = ind$id[as.logical(ind$selected)]
  # todrop = "edit_id"

  # when index argument is TRUE return index rather than actual features
  if(index) {
    return(as.numeric(indx))
  }

  # return selected features
  return(x[as.numeric(indx), !names(x) %in% "edit_id"])
}

#' @name selectFeatures
#' @export
selectFeatures.Spatial = function(
  x = NULL,
  platform = c("mapview", "leaflet"),
  index = FALSE,
  viewer = shiny::paneViewer(),
  ...
) {
  selectFeatures(sf::st_as_sf(x), ...)
}
