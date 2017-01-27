library(igraph)
library(leaflet)
library(mapedit)

karate <- graph.famous("Zachary")
igrf_layout <- layout.auto(karate)

# see a default plot with our layout
plot(karate, layout=igrf_layout)

# plot with leaflet
lf <- leaflet(
  igrf_layout,
  options = leafletOptions(
    crs = leafletCRS(crsClass = "L.CRS.Simple")
  )
) %>%
  addCircleMarkers(group = "network")

new_layout <- lf %>%
  edit_map("network")

# this gets real tricky
#   but we will find a much easier way in mapedit
#   eventually
library(shiny)
shinyApp(
  htmlwidgets::onRender(
    lf,
"
function(el,x) {
  var lf = this;
  setTimeout(
    function(){
      Shiny.onInputChange(
        'getpoints',
        Object.keys(lf.layerManager.getLayerGroup('network')._layers)
      )
    },
    500
  )
}
"
  ),
  function(input, output) {
    observeEvent(input$getpoints, {leafids <<- input$getpoints})
  }
)

# now use our hacked method of id retrieval to identify points
library(dplyr)
library(purrr)

layout_df <- data.frame(igrf_layout) %>%
  mutate(leafid = leafids) %>%
  left_join(
    map_df(
      new_layout$edited[[1]]$features,
      ~data.frame(
        "leafid" = as.character(.x$properties["_leaflet_id"]),
        "newx" = .x$geometry$coordinates[[1]],
        "newy" = .x$geometry$coordinates[[2]],
        stringsAsFactors = FALSE
      )
    ) %>%
      set_names(c("leafid", "newx", "newy"))
  )

layout_df <- layout_df %>%
  mutate(X1 = ifelse(is.na(newx),X1,newx)) %>%
  mutate(X2 = ifelse(is.na(newy), X2, newy))

plot(karate, layout=data.matrix(layout_df[,1:2]))
