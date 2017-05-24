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
      targetGroups = targetGroups,
      ns = session$ns(NULL)
    )
  })

  id = "mapedit"
  select_evt = paste0(id, "_selected")

  df <- data.frame()

  # a container for our selections
  selections <- reactive({
    if(nrow(df) == 0) {
      df <<- data.frame(
        group = input[[select_evt]]$group,
        selected = input[[select_evt]]$selected,
        stringsAsFactors = FALSE
      )
    } else {
      # see if already exists
      loc <- which(df$group == input[[select_evt]]$group)

      if(length(loc) > 0) {
        df[loc, "selected"] <<- input[[select_evt]]$selected
      } else {
        df[nrow(df) + 1, ] <<- c(input[[select_evt]]$group, input[[select_evt]]$selected)
      }
    }

    return(df)
  })

  return(selections)

}


ui <- tagList(
  selectModUI("test-mod"),
  DT::dataTableOutput("selected")
)
server <- function(input, output, session) {
  selections <- callModule(selectMod, "test-mod", lf)
  output$selected <- DT::renderDataTable({DT::datatable(selections())})
  observe({str(selections())})
}
shinyApp(ui, server)



# now try to do selectMap using the  module
library(shiny)
library(htmltools)
library(mapedit)

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
    selections <- callModule(
      selectMod,
      ns,
      x,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      targetGroups = targetGroups
    )

    observe({selections()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        selections()
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


sel_df <- selectMapModule(x=lf)

