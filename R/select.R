#' Interactively Select Map Features
#'
#' @param x features to select
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
selectFeatures = function(x, ...) {
  UseMethod("selectFeatures")
}


#' @name selectFeatures
#' @param mode one of "click" or "draw".
#' @param op the geometric binary predicate to use for the selection.
#'           Can be any of \code{\link{geos_binary_pred}}. In the spatial
#'           operation the drawn features will be evaluated as x and the supplied
#'           feature as y. Ignored if \code{mode = "click"}.
#' @param map a background \code{leaflet} or \code{mapview} map
#'          to be used for editing. If \code{NULL} a blank
#'          mapview canvas will be provided.
#' @param index \code{logical} with \code{index=TRUE} indicating return
#'          the index of selected features rather than the actual
#'          selected features
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#' @param label \code{character} vector or \code{formula} for the
#'          content that will appear in label/tooltip.
#' @export
selectFeatures.sf = function(
  x = NULL,
  mode = c("click", "draw"),
  op = sf::st_intersects,
  map = NULL,
  index = FALSE,
  viewer = shiny::paneViewer(),
  label = NULL,
  ...
) {

  nm = deparse(substitute(x))
  x = mapview:::checkAdjustProjection(x)
  x$edit_id = as.character(1:nrow(x))

  if (is.null(map)) {
    map = mapview::mapview()@map
    map = mapview::addFeatures(
      map, data=x, layerId=~x$edit_id, label=label
    )
    ext = mapview:::createExtent(x)
    map = leaflet::fitBounds(
      map,
      lng1 = ext[1],
      lat1 = ext[3],
      lng2 = ext[2],
      lat2 = ext[4]
    )
    map = mapview::addHomeButton(map = map, ext = ext)
  } else {
    if(inherits(map, "mapview")) {
      map = map@map
    }
    map = mapview::addFeatures(
      map, data=x, layerId=~x$edit_id, label=label
    )
  }

  if (mode[1] == "click") {

    ind = selectMap(map, viewer=viewer, ...)

    indx = ind$id[as.logical(ind$selected)]
    # todrop = "edit_id"

    # when index argument is TRUE return index rather than actual features
    if(index) {
      return(as.numeric(indx))
    }

    # return selected features
    return(x[as.numeric(indx), !names(x) %in% "edit_id"])

  } else {

    stopifnot(requireNamespace("sf"))

    drawn = editMap(mapview::mapview(x, layer.name = nm))

    if (is.null(drawn)) invisible(return(NULL))

    indx = unique(unlist(suppressMessages(op(drawn$finished, x))))

    if(index) {
      return(as.numeric(indx))
    }

    return(x[indx, ])

  }
}

#' @name selectFeatures
#' @export
selectFeatures.Spatial = function(x, ...) {
  selectFeatures(x=sf::st_as_sf(x), ...)
}
