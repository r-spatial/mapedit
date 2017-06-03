#' Interactively Edit a Map
#'
#' @param x map to edit
#'
#' @param ... other arguments
#'
#' @examples
#' library(leaflet)
#' library(mapedit)
#' editMap(leaflet() %>% addTiles())
#'
#' @example inst/examples/examples_edit.R
#' @export
editMap <- function(x, ...) {
  UseMethod("editMap")
}

#' @export
editMap.leaflet <- function(x = NULL, targetLayerId = NULL, sf = TRUE, ns = "mapedit-edit") {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(editModUI(ns), height=NULL, width=NULL),
    miniUI::gadgetTitleBar("Edit Map", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
  )

  server <- function(input, output, session) {
    crud <- callModule(
      editMod,
      ns,
      x,
      targetLayerId = targetLayerId,
      sf = sf
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
    viewer =  shiny::dialogViewer("Draw and Edit"),
    stopOnCancel = FALSE
  )
}

#' @export
editMap.mapview <- function(x = NULL, targetLayerId = NULL, sf = TRUE, ns = "mapedit-edit") {
  stopifnot(!is.null(x), inherits(x, "mapview"), inherits(x@map, "leaflet"))

  editMap.leaflet(x@map, targetLayerId = targetLayerId, sf = sf, ns = ns)
}
