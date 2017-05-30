library(mapedit)
library(mapview)
library(shiny)

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




