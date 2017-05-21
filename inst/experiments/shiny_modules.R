selectModUI <- function(id, ...) {
  ns <- NS(id)
  leafletOutput(ns("map"), ...)
}

selectMod <- function(
  input, output, session,
  leafmap,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  targetGroups = NULL
) {

  output$map <- renderLeaflet({
    mapedit:::add_select_script(
      leafmap,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      targetGroups = targetGroups
    )
  })

  id = "mapedit"
  select_evt = paste0(id, "_selected")

  # a container for our selections
  selections <- reactiveValues(df = data.frame())
  observe({print(input[[select_evt]])})

  observeEvent(input[[select_evt]], {
    #print(input[[select_evt]])
    if(nrow(selections$df) == 0) {
      selections$df <<- data.frame(
        group = input[[select_evt]]$group,
        selected = input[[select_evt]]$selected,
        stringsAsFactors = FALSE
      )
    } else {
      # see if already exists
      loc <- which(selections$df$group == input[[select_evt]]$group)

      if(length(loc) > 0) {
        selections$df[loc, "selected"] <<- input[[select_evt]]$selected
      } else {
        selections$df[nrow(selections) + 1, ] <<- c(input[[select_evt]]$group, input[[select_evt]]$selected)
      }
    }
  })

  return(selections)
}


ui <- tagList(
  selectModUI("test-mod"),
  textOutput("selected")
)
server <- function(input, output, session) {
  selections <- callModule(selectMod, "test-mod", lf)
  output$selected <- renderText({str(selections$df)})
  observe({str(selections$df)})
}
shinyApp(ui, server)



# now try to do selectMap using the  module
selectMapModule <- function(
  x = NULL,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  targetGroups = NULL,
  ns = "mapedit-select"
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(selectModUI(ns), height=NULL, width=NULL),
    miniUI::gadgetTitleBar("Select Features on Map", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
  )

  server <- function(input, output, session) {
    callModule(
      selectMod,
      ns,
      x,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      targetGroups = targetGroups
    )


    shiny::observeEvent(input$done, {
      shiny::stopApp(
        selections
      )
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }


  shiny::runGadget(
    ui,
    server,
    viewer =  shiny::dialogViewer("Select"),
    stopOnCancel = FALSE
  )
}
