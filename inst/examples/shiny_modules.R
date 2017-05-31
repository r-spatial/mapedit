\dontrun{

library(mapedit)
library(mapview)
library(shiny)

# select as a module
m = leaflet(breweries91) %>%
  addCircleMarkers(weight = 1, group = ~as.character(1:nrow(breweries91)))

ui <- tagList(
  selectModUI("test-mod"),
  DT::dataTableOutput("selected")
)
server <- function(input, output, session) {
  selections <- callModule(selectMod, "test-mod", m)
  output$selected <- DT::renderDataTable({DT::datatable(selections())})
  observe({str(selections())})
}
shinyApp(ui, server)

# edit as a module
library(mapedit)
library(mapview)
library(shiny)

m = mapview(breweries91)@map
testsf = NULL
ui <- tagList(
  editModUI("test-edit"),
  h1("What You Draw"),
  leafletOutput("edited")
)
server <- function(input, output, session) {
  crud <- callModule(editMod, "test-edit", m, "breweries91")
  output$edited <- renderLeaflet({
    req(crud()$finished)
    mapview(crud()$finished)@map
  })
}
shinyApp(ui, server)

}
