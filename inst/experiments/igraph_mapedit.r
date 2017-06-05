library(igraph)
library(mapview)
library(mapedit)
library(sf)


karate <- graph.famous("Zachary")
igrf_layout <- layout.auto(karate)

# see a default plot with our layout
plot(karate, layout=igrf_layout)

geom <- st_cast(
  st_sfc(
    st_multipoint(igrf_layout)
  ),
  "POINT"
)
igrf_sf <- st_sf(geom)

# plot with sf to confirm tranform occurred correctly
plot(igrf_sf)

# plot with leaflet
lf <- leaflet(
  igrf_sf,
  options = leafletOptions(
    crs = leafletCRS(crsClass = "L.CRS.Simple")
  )
) %>%
  addCircleMarkers(group = "network")

new_layout <- lf %>%
  editMap("network")

# this gets real tricky
#   but we will find a much easier way in mapedit
#   eventually
library(shiny)
app <- shinyApp(
  ui = editModUI("mapeditor"),
  server = function(input, output, session) {
    edits = callModule(
      editMod,
      "mapeditor",
      htmlwidgets::onRender(
        lf,
"
function(el,x) {
  var lf = this;
  setTimeout(
    function(){
      Shiny.onInputChange(
      'getpoints',
      Object.keys(lf.layerManager._byGroup.network)
      )
    },
    500
  )
}
"
      ),
      targetLayerId = "network"
    )

    observeEvent(input$getpoints, {stopApp(input$getpoints)})
  }
)

leafids <- runApp(app)

# now use our hacked method of id retrieval to identify points
library(dplyr)

layout_sf <- igrf_sf %>%
  mutate(leafid = as.numeric(leafids)) %>%
  {
    .[which(.$leafid %in% new_layout$edited$X_leaflet_id),]$geom <- new_layout$edited$feature
    .
  }

mapview(layout_sf)

plot(karate, layout=data.matrix(layout_df[,1:2]))
