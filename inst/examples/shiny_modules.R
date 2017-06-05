\dontrun{

library(mapedit)
library(mapview)
library(shiny)

# select as a module
m = leaflet(breweries91) %>%
  addCircleMarkers(weight = 1, layerId = 1:nrow(breweries91))

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


# editMap module can easily be combined to make a selection tool
#   do selection of breweries with drawn polygons
library(sf)
library(mapview)
library(mapedit)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6,editModUI("brew-select")),
    column(6,leafletOutput("mapout"))
  )
)
server <- function(input,output,session) {
  m = mapview(breweries91)@map
  brew_sf <- st_as_sf(breweries91)
  drawn <- callModule(editMod, "brew-select", m)
  calc_sf <- reactiveValues()
  observe({
    req(drawn()$finished)
    calc_sf$intersection <- st_intersection(drawn()$finished, brew_sf)
  })
  output$mapout <- renderLeaflet({
    req(calc_sf$intersection)
    (mapview(calc_sf$intersection) + mapview(drawn()$finished))@map
  })
}
shinyApp(ui,server)

}

