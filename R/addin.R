


#' @title mapedit create features Addin
#' @description Create and save spatial objects within the Rstudio IDE. Objects
#' can then be saved to file types such as \code{.geojson} or \code{.shp}.
#' Objects are also output to the console and can be assigned to a variable
#' using `.Last.value`. If you wish to pass the output directly to a variable
#' simply call the addin function, ie. \code{new_sf <- createFeatures()}.
#'
#' An existing sf \code{data.frame} can also be passed either indirectly by
#' selecting text in RStudio with the name of the object, or directly by
#' passing the existing sf object to \code{new_sf <- createFeatures(existing_sf)}.
#' When passing an existing sf object you can only add and edit additional features,
#' the existing features cannot be changed.
#'
#' @param SF_OBJECT sf Simple feature collection
#'
#' @return sf object and/or saved to file
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniButtonBlock
#' @import shiny
#' @importFrom shinyWidgets switchInput updateSwitchInput
#' @importFrom mapview mapview
#' @importFrom sf write_sf
#' @importFrom dplyr bind_rows
#' @importFrom leaflet setView
#' @importFrom rstudioapi getActiveDocumentContext
#' @export
#'
createFeatures <- function(SF_OBJECT = NULL) {

  ui <- miniPage(
    gadgetTitleBar("Edit Map"),
    miniContentPanel(
      editModUI("editor"),
      miniButtonBlock(
        div(style="display: inline-block;padding-top:22px;padding-left:30px;width:200px;",
            switchInput('savefile', 'Save', value = FALSE, onStatus = "success", offStatus = "danger")),
        div(style="display: inline-block; width: 400px;",
            textInput('filename', '', value = 'saved_geometry.geojson')),
        div(style="display: inline-block;padding-top:18px;width: 400px;font-size: 10pt;color: #313844;",
            'The filename can include a path relative to working directory. ',
            'A different file type can be selected by changing the file extension.')
      )
    )
  )

  server <- function(input, output, session) {

    # get values from rstudio
    ct <- rstudioapi::getActiveDocumentContext()

    TEXT <- ct$selection[[1]]$text
    OBJECTNAME <- ifelse(TEXT == '', 'geom', TEXT)
    FILENAME <- ifelse(TEXT == '', 'saved_geometry.geojson', paste0(TEXT, '.geojson'))

    # test selected text an sf object (if not passed directly)
    try({
      if (is.null(SF_OBJECT)) {
        SF_OBJECT <- get(TEXT)
        if (!('sf' %in% class(SF_OBJECT))) {SF_OBJECT <- NULL}
      }
    }, silent = TRUE)

    # update UI based on inputs
    updateTextInput(session, 'filename', value = FILENAME)
    if (FILENAME != 'saved_geometry.geojson') {
      updateSwitchInput(session, 'savefile', value = TRUE)
    }

    # load mapedit
    if ('sf' %in% class(SF_OBJECT)) {
      geo <- callModule(editMod, "editor", mapview(SF_OBJECT)@map)
    } else {
      geo <- callModule(editMod, "editor", setView(mapview()@map, 80, 0, 3))
    }

    observe({
      input$filename
      OBJECTNAME <- tools::file_path_sans_ext(basename(input$filename))
    })

    # return geometry to file and object in console
    observeEvent(input$done, {
      geom <- geo()$finished

      if (!is.null(geom) & !is.null(SF_OBJECT)) geom <- dplyr::bind_rows(SF_OBJECT, geom)

      if (!is.null(geom)) {
        if (input$savefile) {
          sf::write_sf(geom, input$filename, delete_layer = TRUE, delete_dsn = TRUE)
        }
      }

      stopApp({
        if (!is.null(geom)) {
          geom
        }
      })
    })

  }

  viewer <- paneViewer(600)
  runGadget(ui, server, viewer = viewer)

}
