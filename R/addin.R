


#' @title mapedit Addin
#' @description Create and save spatial objects within the Rstudio IDE
#'
#' @return sf object to .GlobalEnv and/or saved to file
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniButtonBlock
#' @import shiny
#' @importFrom shinyWidgets switchInput updateSwitchInput
#' @importFrom mapview mapview
#' @importFrom sf write_sf
#' @importFrom leaflet setView
#' @importFrom rstudioapi getActiveDocumentContext
#' @export
#'
mapeditAddin <- function() {

  ui <- miniPage(
    gadgetTitleBar("Edit Map"),
    miniContentPanel(
      editModUI("editor"),
      miniButtonBlock(
        div(style="display: inline-block;padding-top:22px;padding-left:30px;width:180px;",
            switchInput('savefile', 'Save', value = FALSE, onStatus = "success", offStatus = "danger")),
        div(style="display: inline-block; width: 400px;",
            textInput('filename', '', value = 'saved_geometry.geojson')),
        div(style="display: inline-block;padding-top:18px;width: 400px;font-size: 10pt;color: #313844;",
            'You can add folders and change output type.',
            'Created geometry will always save to .GlobalEnv')
      )
    )
  )

  server <- function(input, output, session) {

    # get values from rstudio
    ct <- getActiveDocumentContext()

    TEXT <- ct$selection[[1]]$text
    OBJECTNAME <- ifelse(TEXT == '', 'geom', TEXT)
    FILENAME <- ifelse(TEXT == '', 'saved_geometry.geojson', paste0(TEXT, '.geojson'))
    SF_OBJECT <- NULL

    # test selected text an sf object
    try({
      SF_OBJECT <- get(TEXT)
      if (class(SF_OBJECT) != 'sf') {SF_OBJECT <- NULL}
    })

    # update UI based on inputs
    updateTextInput(session, 'filename', value = FILENAME)
    if (FILENAME != 'saved_geometry.geojson') {
      updateSwitchInput(session, 'savefile', value = TRUE)
    }


    # load mapedit
    if (class(SF_OBJECT) == 'sf') {
      geo <- callModule(editMod, "editor", mapview(SF_OBJECT)@map)
    } else {
      geo <- callModule(editMod, "editor", setView(mapview()@map, 80, 0, 3))
    }


    # return geometry to file and object in .GlobalEnv
    observeEvent(input$done, {
      geom <- geo()$finished

      if (!is.null(geom) & !is.null(SF_OBJECT)) geom <- rbind(SF_OBJECT, geom)

      if (!is.null(geom)) {
        assign(OBJECTNAME, geom, envir = .GlobalEnv)
        if (input$savefile) {
          sf::write_sf(geom, input$filename, delete_layer = TRUE, delete_dsn = TRUE)
        }
      }

      stopApp()
    })

  }

  viewer <- paneViewer(600)
  runGadget(ui, server, viewer = viewer)

}
