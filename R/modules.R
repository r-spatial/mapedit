#' Shiny Module UI for Geo Selection
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @return ui for Shiny module
#' @export
selectModUI <- function(id, ...) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("map"), ...)
}


#' Shiny Module Server for Geo Selection
#'
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param leafmap leaflet map to use for Selection
#' @param styleFalse named \code{list} of valid \code{CSS} for non-selected features
#' @param styleTrue named \code{list} of valid \code{CSS} for selected features
#'
#' @return server function for Shiny module
#' @import shiny
#' @export
selectMod <- function(
  input, output, session,
  leafmap,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7)
) {

  output$map <- leaflet::renderLeaflet({
    add_select_script(
      leafmap,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      ns = session$ns(NULL)
    )
  })

  id = "mapedit"
  select_evt = paste0(id, "_selected")

  df <- data.frame()

  # a container for our selections
  selections <- reactive({
    # when used in modules, we get an event with blank id
    #  on initialize so also make sure we have an id
    id = as.character(input[[select_evt]]$id)
    if(nrow(df) == 0 && !is.null(id)) {
      df <<- data.frame(
        id = id,
        selected = input[[select_evt]]$selected,
        stringsAsFactors = FALSE
      )
    } else {
      # see if already exists
      loc <- which(df$id == id)

      if(length(loc) > 0) {
        df[loc, "selected"] <<- input[[select_evt]]$selected
      } else {
        df[nrow(df) + 1, ] <<- c(id, input[[select_evt]]$selected)
      }
    }

    return(df)
  })

  return(selections)

}


#' Shiny Module UI for Geo Create, Edit, Delete
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @return ui for Shiny module
#' @import shiny
#' @export
editModUI <- function(id, ...) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("map"), ...)
}

#' Shiny Module Server for Geo Create, Edit, Delete
#'
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param leafmap leaflet map to use for Selection
#' @param targetLayerId \code{character} identifier of layer to edit, delete
#' @param sf \code{logical} to return simple features.  \code{sf=FALSE} will return
#'          \code{GeoJSON}.
#' @param record \code{logical} to record all edits for future playback.
#' @param crs see \code{\link[sf]{st_crs}}.
#' @param editor \code{character} either "leaflet.extras" or "leafpm"
#'
#' @return server function for Shiny module
#' @import shiny
#' @export
editMod <- function(
  input, output, session,
  leafmap,
  targetLayerId = NULL,
  sf = TRUE,
  record = FALSE,
  crs = 4326,
  editor = c("leaflet.extras", "leafpm")
) {
  # check to see if addDrawToolbar has been already added to the map
  if(is.null(
    Find(
      function(cl) {
        cl$method == "addDrawToolbar" || cl$method == "addPmToolbar"
      },
      leafmap$x$calls
    )
  )) {
    if(editor[1] == "leaflet.extras") {
      # add draw toolbar if not found
      leafmap <- leaflet.extras::addDrawToolbar(
        leafmap,
        targetGroup = targetLayerId,
        polylineOptions = leaflet.extras::drawPolylineOptions(repeatMode = TRUE),
        polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
        circleOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
        markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
        circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(repeatMode = TRUE),
        editOptions = leaflet.extras::editToolbarOptions()
      )
    }

    if(editor[1] == "leafpm") {
      leafmap <- leafpm::addPmToolbar(
        leafmap,
        targetGroup = targetLayerId,
        toolbarOptions = leafpm::pmToolbarOptions(drawCircle = FALSE)
      )
    }
  }

  output$map <- leaflet::renderLeaflet({leafmap})

  featurelist <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list(),
    all = list()
  )

  recorder <- list()

  EVT_DRAW <- "map_draw_new_feature"
  EVT_EDIT <- "map_draw_edited_features"
  EVT_DELETE <- "map_draw_deleted_features"
  EVT_ALL <- "map_draw_all_features"

  shiny::observeEvent(input[[EVT_DRAW]], {
    featurelist$drawn <- c(featurelist$drawn, list(input[[EVT_DRAW]]))
    if (any(unlist(input[[EVT_DRAW]]$geometry$coordinates) < -180) ||
        any(unlist(input[[EVT_DRAW]]$geometry$coordinates) > 180))
      insane_longitude_warning()
    featurelist$finished <- c(featurelist$finished, list(input[[EVT_DRAW]]))
  })

  shiny::observeEvent(input[[EVT_EDIT]], {
    edited <- input[[EVT_EDIT]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist$finished[loc] <<- list(x)
      }
    })

    featurelist$edited_all <- c(featurelist$edited_all, list(edited))
  })

  shiny::observeEvent(input[[EVT_DELETE]], {
    deleted <- input[[EVT_DELETE]]

    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))

    # leaflet.pm returns only a single feature while leaflet.extras returns feature collection
    # convert leaflet.pm so logic will be the same
    if(editor == "leafpm") {
      deleted <- list(
        type = "FeatureCollection",
        features = list(deleted)
      )
    }

    # now modify finished to match edited
    lapply(deleted$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist$finished[loc] <<- NULL
      }
    })

    featurelist$deleted_all <- c(featurelist$deleted_all, list(deleted))
  })

  shiny::observeEvent(input[[EVT_ALL]], {
    featurelist$all <- list(input[[EVT_ALL]])
    if (any(unlist(input[[EVT_ALL]]$geometry$coordinates) < -180) ||
        any(unlist(input[[EVT_ALL]]$geometry$coordinates) > 180))
      insane_longitude_warning()
  })

  # record events if record = TRUE
  if(record == TRUE) {
    lapply(
      c(EVT_DRAW, EVT_EDIT, EVT_DELETE, EVT_ALL),
      function(evt) {
        observeEvent(input[[evt]], {
          recorder <<- c(
            recorder,
            list(
              list(
                event = evt,
                timestamp = Sys.time(),
                feature = input[[evt]]
              )
            )
          )
        })
      }
    )
  }


  # collect all of the the features into a list
  #  by action
  returnlist <- reactive({
    workinglist <- list(
      drawn = featurelist$drawn,
      edited = featurelist$edited_all,
      deleted = featurelist$deleted_all,
      finished = featurelist$finished,
      all = featurelist$all
    )
    # if sf argument is TRUE then convert to simple features
    if(sf) {
      workinglist <- lapply(
        workinglist,
        function(action) {
          # ignore empty action types to prevent error
          #   handle in the helper functions?
          if(length(action) == 0) { return() }

          # FeatureCollection requires special treatment
          #  and we need to extract features
          features <- Reduce(
            function(left,right) {
              if(right$type == "FeatureCollection") {
                right <- lapply(right$features, identity)
              } else {
                right <- list(right)
              }
              c(left,right)
            },
            action,
            init = NULL
          )

          combine_list_of_sf(
            lapply(features, st_as_sf.geo_list, crs = crs)
          )
        }
      )

      recorder <- lapply(
        recorder,
        function(evt) {
          feature = st_as_sfc.geo_list(evt$feature, crs = crs)
          list(evt = evt$event, timestamp = evt$timestamp, feature = feature)
        }
      )
    }
    # return merged features
    if(record==TRUE) {
      attr(workinglist, "recorder") <- recorder
    }
    return(workinglist)
  })

  return(returnlist)
}
