

##' @title Prepare arguments for addDrawToolbar or addPmToolbar
##' @param fun Function used by editor package (leafpm or
##'     leaflet.extras) to set defaults
##' @param args Either a (possibly nested) list of named options of
##'     the form suitable for passage to \code{fun} or (if the chosen
##'     editor is \code{"leaflet.extras"}) \code{FALSE}.
##' @return An object suitable for passing in as the supplied argument
##'     to either \code{leaflet.extras::addDrawToolbar} or
##'     \code{leafpm::addPmToolbar}.
processOpts <- function(fun, args) {
    ## Account for special meaning of `FALSE` as arg in leaflet.extras
    if(identical(args, FALSE)) {
        return(FALSE)
    } else {
        return(do.call(fun, args))
    }
}


##' @title Add a (possibly customized) toolbar to a leaflet map
##' @param leafmap leaflet map to use for Selection
##' @param editorOptions A list of options to be passed on to either
##'     \code{leaflet.extras::addDrawToolbar} or
##'     \code{leafpm::addPmToolbar}.
##' @param editor Character string giving editor to be used for the
##'     current map. Either \code{"leafpm"} or
##'     \code{"leaflet.extras"}.
##' @param targetLayerId \code{string} name of the map layer group to
##'     use with edit
##' @return The leaflet map supplied to \code{leafmap}, now with an
##'     added toolbar.
addToolbar <- function(leafmap, editorOptions, editor,
                       targetLayerId) {
    ## Set up this package's defaults
    if (editor == "leafpm") {
        if(any(sapply(leafmap$x$calls, "[[", "method") %in%
               c("addPolylines", "addPolygons"))) {
            editorDefaults <-
                list(toolbarOptions = list(drawCircle = FALSE),
                     drawOptions = list(allowSelfIntersection = FALSE),
                     editOptions = list(allowSelfIntersection = FALSE),
                     cutOptions = list(allowSelfIntersection = FALSE))
        } else {
            editorDefaults <-
                list(toolbarOptions = list(drawCircle = FALSE),
                     drawOptions = list(),
                     editOptions = list(),
                     cutOptions = list())
        }
    }
    if (editor == "leaflet.extras") {
        editorDefaults <-
            list(polylineOptions = list(repeatMode = TRUE),
                 polygonOptions = list(repeatMode = TRUE),
                 circleOptions = FALSE,
                 rectangleOptions = list(repeatMode = TRUE),
                 markerOptions = list(repeatMode = TRUE),
                 circleMarkerOptions = list(repeatMode = TRUE),
                 editOptions = list())
    }

    ## Apply user-supplied options, if any
    editorArgs <- utils::modifyList(editorDefaults, editorOptions)


    ## Add toolbar to leafmap object
    if (editor == "leaflet.extras") {
        leaflet.extras::addDrawToolbar(
          leafmap,
          targetGroup = targetLayerId,
          polylineOptions =
              processOpts(leaflet.extras::drawPolylineOptions,
                          editorArgs[["polylineOptions"]]),
          polygonOptions =
              processOpts(leaflet.extras::drawPolygonOptions,
                          editorArgs[["polygonOptions"]]),
          circleOptions =
              processOpts(leaflet.extras::drawCircleOptions,
                          editorArgs[["circleOptions"]]),
          rectangleOptions =
              processOpts(leaflet.extras::drawRectangleOptions,
                          editorArgs[["rectangleOptions"]]),
          markerOptions =
              processOpts(leaflet.extras::drawMarkerOptions,
                             editorArgs[["markerOptions"]]),
          circleMarkerOptions =
              processOpts(leaflet.extras::drawCircleMarkerOptions,
                          editorArgs[["circleMarkerOptions"]]),
          editOptions =
              processOpts(leaflet.extras::editToolbarOptions,
                          editorArgs[["editOptions"]])
          )
    } else if (editor == "leafpm") {
        leafpm::addPmToolbar(
          leafmap,
          targetGroup = targetLayerId,
          toolbarOptions = processOpts(leafpm::pmToolbarOptions,
                                       editorArgs[["toolbarOptions"]]),
          drawOptions = processOpts(leafpm::pmDrawOptions,
                                    editorArgs[["drawOptions"]]),
          editOptions = processOpts(leafpm::pmEditOptions,
                                    editorArgs[["editOptions"]]),
          cutOptions = processOpts(leafpm::pmCutOptions,
                                   editorArgs[["cutOptions"]])
          )
    }
}

