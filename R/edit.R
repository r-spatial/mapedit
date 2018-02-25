#' Interactively Edit a Map
#'
#' @param x \code{leaflet} or \code{mapview} map to edit
#' @param ... other arguments for \code{mapview::addFeatures()} when
#'          using \code{editMap.NULL} or \code{selectFeatures}
#'
#' @return \code{sf} simple features or \code{GeoJSON}
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(mapedit)
#' editMap(leaflet() %>% addTiles())
#' }
#' @example inst/examples/examples_edit.R
#' @export
editMap <- function(x, ...) {
  UseMethod("editMap")
}

#' @name editMap
#' @param targetLayerId \code{string} name of the map layer group to use with edit
#' @param sf \code{logical} return simple features.  The default is \code{TRUE}.
#'          If \code{sf = FALSE}, \code{GeoJSON} will be returned.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param record \code{logical} to record all edits for future playback.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#' @export
editMap.leaflet <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  ...
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      editModUI(id = ns, height="97%"),
      height=NULL, width=NULL
    ),

    miniUI::gadgetTitleBar(title = title,
                           right = miniUI::miniTitleBarButton("done", "Done",
                                                              primary = TRUE))
  )

  server <- function(input, output, session) {
    crud <- callModule(
      editMod,
      ns,
      x,
      targetLayerId = targetLayerId,
      sf = sf,
      record = record,
      crs = crs
    )

    observe({crud()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        crud()
      )
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }

  shiny::runGadget(
    ui,
    server,
    viewer =  viewer,
    stopOnCancel = FALSE
  )
}

#' @name editMap
#' @export
editMap.mapview <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  ...
) {
  stopifnot(!is.null(x), inherits(x, "mapview"), inherits(x@map, "leaflet"))

  editMap.leaflet(
    x@map, targetLayerId = targetLayerId, sf = sf,
    ns = ns, viewer = viewer, record = TRUE, crs = crs,
    title = title
  )
}

#' @name editMap
#' @export
editMap.NULL = function(x, ...) {
  m = mapview::mapview()@map
  m = leaflet::fitBounds(
    m,
    lng1 = -180, #as.numeric(sf::st_bbox(x)[1]),
    lat1 = -90, #as.numeric(sf::st_bbox(x)[2]),
    lng2 = 180, #as.numeric(sf::st_bbox(x)[3]),
    lat2 = 90 #as.numeric(sf::st_bbox(x)[4])
  )
  ed = editMap(m, record=TRUE)
  ed_record <- ed$finished
  attr(ed_record, "recorder") <- attr(ed, "recorder", exact = TRUE)
  ed_record
}


#' Interactively Edit Map Features
#'
#' @param x features to edit
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
editFeatures = function(x, ...) {
  UseMethod("editFeatures")
}

#' @name editFeatures
#'
#' @param map a background \code{leaflet} or \code{mapview} map
#'          to be used for editing. If \code{NULL} a blank
#'          mapview canvas will be provided.
#' @param mergeOrder \code{vector} or \code{character} arguments to specify the order
#'          of merge operations.  By default, merges will proceed in the order
#'          of add, edit, delete.
#' @param record \code{logical} to record all edits for future playback.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#' @param label \code{character} vector or \code{formula} for the
#'          content that will appear in label/tooltip.
#' @export
editFeatures.sf = function(
  x,
  map = NULL,
  mergeOrder = c("add", "edit", "delete"),
  record = FALSE,
  viewer = shiny::paneViewer(),
  crs = 4326,
  label = NULL,
  ...
) {

  x$edit_id = as.character(1:nrow(x))

  if (is.null(map)) {
    x = mapview:::checkAdjustProjection(x)
    map = mapview::mapview()@map
    map = mapview::addFeatures(
      map, data=x, layerId=~x$edit_id,
      label=label,
      labelOptions = leaflet::labelOptions(direction="top", offset=c(0,-40)),
      group = "toedit"
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
      map, data=x, layerId=~x$edit_id,
      label=label,
      labelOptions = leaflet::labelOptions(direction="top", offset=c(0,-40)),
      group = "toedit"
    )
  }

  crud = editMap(
    map, targetLayerId = "toedit",
    viewer = viewer, record = record,
    crs = crs, ...
  )

  merged <- Reduce(
    function(left_sf, op) {
      op <- tolower(op)
      if(op == "add") sf_merge <- crud$finished
      if(op == "edit") sf_merge <- crud$edited
      if(op == "delete") sf_merge <- crud$deleted

      if(is.null(sf_merge)) return(left_sf)

      # will need to rethink this but for now
      #   since we use finished above
      #   only apply edit and delete
      #   where an edit_id is available
      #   meaning only to a feature in the original sf
      if(op %in% c("edit", "delete")) {
        # if layerId column does not exist then all are new features
        #   and should already be handled in finished
        if(!("layerId" %in% colnames(sf_merge))) {
          return(left_sf)
        }
        # ignore any with NA as layerId since these will also be
        #  handled in finished
        sf_merge <- sf_merge[which(!is.na(sf_merge$layerId)),]
      }

      if(nrow(sf_merge) == 0) return(left_sf)

      eval(call(paste0("merge_", op), left_sf, sf_merge, c("edit_id" = "layerId")))
    },
    mergeOrder,
    init = x
  )

  merged <- dplyr::select_(merged, "-edit_id")

  # return merged features
  if(record==TRUE) {
    attr(merged, "recorder") <- attr(crud, "recorder", exact=TRUE)
    attr(merged, "original") <- x
  }

  merged
}

#' @name editFeatures
#' @export
editFeatures.Spatial = function(x, ...) {
  editFeatures(sf::st_as_sf(x), ...)
}
