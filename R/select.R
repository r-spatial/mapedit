#' Interactively Select Map Features
#'
#' @param x map to use
#'
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
selectFeatures <- function(x, ...) {
  UseMethod("selectFeatures")
}

#' @export
selectFeatures.sf <- function(x, ...) {
  x$edit_group = as.character(1:nrow(x))

  addfun = switch(as.character(sf::st_dimension(sf::st_geometry(x)))[1],
                  "0" = leaflet::addCircleMarkers,
                  "1" = leaflet::addPolylines,
                  "2" = leaflet::addPolygons)

  m = leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    addfun(data = x, weight = 1, group = ~edit_group)

  ind = select_map(m, ...)

  indx = ind$group[as.logical(ind$selected)]
  todrop = "edit_group"
  return(x[as.numeric(indx), !names(x) %in% todrop])
}

