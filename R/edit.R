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
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param crs see \code{\link[sf]{st_crs}}.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Edit Map".
#' @param editor \code{character} either "leaflet.extras" or "leafpm"
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
editMap.leaflet <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
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
    miniUI::gadgetTitleBar(
      title = title,
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    tags$script(HTML(
      "
// close browser window on session end
$(document).on('shiny:disconnected', function() {
  // check to make sure that button was pressed
  //  to avoid websocket disconnect caused by some other reason than close
  if(
    Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
    Shiny.shinyapp.$inputValues['done:shiny.action']
  ) {
    window.close()
  }
})
"
    ))
  )

  server <- function(input, output, session) {
    crud <- callModule(
      editMod,
      ns,
      x,
      targetLayerId = targetLayerId,
      sf = sf,
      record = record,
      crs = crs,
      editor = editor
    )

    observe({crud()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        crud()
      )
    })

    # if browser viewer and user closes tab/window
    #  then Shiny does not stop so we will stopApp
    #  when a session ends.  This works fine unless a user might
    #  have two sessions open.  Closing one will also close the
    #  other.
    session$onSessionEnded(function() {
      # should this be a cancel where we send NULL
      #  or a done where we send crud()
      shiny::stopApp(isolate(crud()))
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
  editor = c("leaflet.extras", "leafpm"),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "mapview"), inherits(x@map, "leaflet"))

  editMap.leaflet(
    x@map, targetLayerId = targetLayerId, sf = sf,
    ns = ns, viewer = viewer, record = TRUE, crs = crs,
    title = title,
    editor = editor
  )
}

#' @name editMap
#' @export
editMap.NULL = function(x, editor = c("leaflet.extras", "leafpm"), ...) {
  m = mapview::mapview()@map
  m = leaflet::fitBounds(
    m,
    lng1 = -180, #as.numeric(sf::st_bbox(x)[1]),
    lat1 = -90, #as.numeric(sf::st_bbox(x)[2]),
    lng2 = 180, #as.numeric(sf::st_bbox(x)[3]),
    lat2 = 90 #as.numeric(sf::st_bbox(x)[4])
  )
  ed = editMap(m, record = TRUE, editor = editor)
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
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param label \code{character} vector or \code{formula} for the
#'          content that will appear in label/tooltip.
#' @param crs see \code{\link[sf]{st_crs}}.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Edit Map".
#' @param editor \code{character} either "leaflet.extras" or "leafpm"
#' @param pmToolbarOpts \code{list} of supplied arguments to be passed
#'     on to \code{leafpm::pmToolbarOptions}. The list elements (if
#'     any) should be among those documented as accepted by
#'     \code{\link[leafpm]{pmToolbarOptions}}.
#' @param pmDrawOpts \code{list} of supplied arguments to be passed on
#'     to \code{leafpm::pmDrawOptions}. The list elements (if any)
#'     should be among those documented as accepted by
#'     \code{\link[leafpm]{pmDrawOptions}}.
#' @param pmEditOpts \code{list} of supplied arguments to be passed on
#'     to \code{leafpm::pmEditOptions}. The list elements (if any)
#'     should be among those documented as accepted by
#'     \code{\link[leafpm]{pmEditOptions}}.
#' @param pmCutOpts \code{list} of supplied arguments to be passed on
#'     to \code{leafpm::pmCutOptions}. The list elements (if any)
#'     should be among those documented as accepted by
#'     \code{\link[leafpm]{pmCutOptions}}.
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
editFeatures.sf = function(
  x,
  map = NULL,
  mergeOrder = c("add", "edit", "delete"),
  record = FALSE,
  viewer = shiny::paneViewer(),
  crs = 4326,
  label = NULL,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...,
  pmToolbarOpts = list(),
  pmDrawOpts = list(),
  pmEditOpts = list(),
  pmCutOpts = list()
) {

  # store original projection of edited object ----
  orig_proj <- sf::st_crs(x)
  if (is.na(orig_proj)) {
    stop("The CRS of the input object is not set. Aborting. `mapedit` does not currently
         allow editing objects with arbitrary coordinates system. Please set the
         CRS of the input using `sf::st_set_crs()` (for `sf` objects) or `proj4string()
         for `sp` objects", call. = FALSE)
  }

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

  # currently we don't have a way to set custom options for leaflet.pm
  # and we will want to customize allowSelfIntersection depending on feature types
  if(inherits(map, "mapview")) map = map@map
  if(editor[1] == "leafpm") {
    # now let's see if any of the features are polygons
    if(any(sf::st_dimension(x) == 2)) {
      map = leafpm::addPmToolbar(
        map,
        targetGroup = "toedit",
        toolbarOptions =
          do.call(leafpm::pmToolbarOptions,
                  modifyList(list(drawCircle = FALSE), pmToolbarOpts)),
        drawOptions =
          do.call(leafpm::pmDrawOptions,
                  modifyList(list(allowSelfIntersection = FALSE), pmDrawOpts)),
        editOptions =
          do.call(leafpm::pmEditOptions,
                  modifyList(list(allowSelfIntersection = FALSE), pmEditOpts)),
        cutOptions =
          do.call(leafpm::pmCutOptions,
                  modifyList(list(allowSelfIntersection = FALSE), pmCutOpts))
      )
    }
  }

  crud = editMap(
    map, targetLayerId = "toedit",
    viewer = viewer, record = record,
    crs = crs, title = title, editor = editor, ...
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

  # re-transform to original projection if needed ----
  if (sf::st_crs(merged) != orig_proj) {
    merged <- sf::st_transform(merged, orig_proj)
  }

  # warn if anything is not valid
  if(!all(sf::st_is_valid(merged))) {
    warning("returned features do not appear valid; please inspect closely", call. = FALSE)
  }

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
