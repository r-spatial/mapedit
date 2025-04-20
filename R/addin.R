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
#' @export
#'
createFeatures <- function(SF_OBJECT = NULL) {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit Map"),
    miniUI::miniContentPanel(
      mapedit::editModUI("editor"),
      miniUI::miniButtonBlock(
        htmltools::div(style="display: inline-block;padding-top:22px;padding-left:30px;width:200px;",
                       shinyWidgets::switchInput('savefile', 'Save', value = FALSE, onStatus = "success", offStatus = "danger")),
        htmltools::div(style="display: inline-block; width: 400px;",
                       shiny::textInput('filename', '', value = 'saved_geometry.geojson')),
        htmltools::div(style="display: inline-block;padding-top:18px;width: 400px;font-size: 10pt;color: #313844;",
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
    shiny::updateTextInput(session, 'filename', value = FILENAME)
    if (FILENAME != 'saved_geometry.geojson') {
      shinyWidgets::updateSwitchInput(session, 'savefile', value = TRUE)
    }

    # load mapedit
    if ('sf' %in% class(SF_OBJECT)) {
      geo <- shiny::callModule(mapedit::editMod, "editor", mapview::mapview(SF_OBJECT)@map)
    } else {
      geo <- shiny::callModule(mapedit::editMod, "editor", leaflet::setView(mapview::mapview()@map, 80, 0, 3))
    }

    shiny::observe({
      input$filename
      OBJECTNAME <- tools::file_path_sans_ext(basename(input$filename))
    })

    # return geometry to file and object in console
    shiny::observeEvent(input$done, {
      geom <- geo()$finished

      if (!is.null(geom) & !is.null(SF_OBJECT)) {
        geom <- dplyr::bind_rows(SF_OBJECT, geom)
      }

      if (!is.null(geom)) {
        if (input$savefile) {
          sf::write_sf(geom, input$filename, delete_layer = TRUE, delete_dsn = TRUE)
        }
      }

      shiny::stopApp({
        if (!is.null(geom)) {
          geom
        }
      })
    })

  }

  viewer <- shiny::paneViewer(600)
  shiny::runGadget(ui, server, viewer = viewer)

}
