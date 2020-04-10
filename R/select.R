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
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param label \code{character} vector or \code{formula} for the
#'          content that will appear in label/tooltip.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Select features".
#'
#' @details
#'   When setting \code{viewer = browserViewer(browser = getOption("browser"))} and
#'   the systems default browser is Firefox, the browser window will likely not
#'   automatically close when the app is closed (by pressing "done" or "cancel").
#'   To enable automatic closing of tabs/windows in Firefox try the following:
#'   \itemize{
#'     \item{input "about:config " to your firefox address bar and hit enter}
#'     \item{make sure your "dom.allow_scripts_to_close_windows" is true}
#'   }
#'
#' @export
selectFeatures.sf = function(
  x = NULL,
  mode = c("click", "draw"),
  op = sf::st_intersects,
  map = NULL,
  index = FALSE,
  viewer = shiny::paneViewer(),
  label = NULL,
  title = "Select features",
  ...
) {

  nm = deparse(substitute(x))
  x = checkAdjustProjection(x)
  x$edit_id = as.character(1:nrow(x))

  mode = match.arg(mode)

  if (mode == "click") {

    if (is.null(map)) {
      map = mapview:::initMap(proj4str = sf::st_crs(x)$proj4string)
      map = leafem::addFeatures(
        map, data = x, layerId = ~x$edit_id, label = label, ...
      )
      ext = createExtent(x)
      map = leaflet::fitBounds(
        map,
        lng1 = ext[1],
        lat1 = ext[3],
        lng2 = ext[2],
        lat2 = ext[4]
      )
      map = leafem::addHomeButton(map = map, ext = ext)
    } else {
      if(inherits(map, "mapview")) {
        map = map@map
      }
      map = leafem::addFeatures(
        map, data=x, layerId=~x$edit_id, label=label
      )
    }

    ind = selectMap(map, viewer=viewer, title = title, ...)

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

    drawn = editMap(mapview::mapView(x, map = map, layer.name = nm, ...), title = title)

    if (is.null(drawn$finished)) invisible(return(NULL))

    if (!is.na(sf::st_crs(x))) {
      fin = sf::st_transform(drawn$finished, sf::st_crs(x))
    } else {
      fin = drawn$finished
      sf::st_crs(fin) = NA
    }
    indx = unique(unlist(suppressMessages(op(fin, x))))

    if(index) {
      return(as.numeric(indx))
    }

    return(x[indx, ])

  }
}

#' @name selectFeatures
#' @export
selectFeatures.Spatial = function(x, ...) {
  selectFeatures(x = sf::st_as_sf(x), ...)
}
