\dontrun{
  # demonstrate Leaflet.Draw on a layer

  # will use the very new randgeo
  # devtools::install_github("ropensci/randgeo")

  library(leaflet)
  library(leaflet.extras)
  library(randgeo)  # see install instructions above
# not working now due to error in addGeoJSON :(
  lf <- leaflet() %>%
    addTiles() %>%
    addGeoJSON(
      geo_polygon(
        count = 10,
        bbox = c(50,50,60,60),
        max_radial_length = 2
      ),
      layerId = "randgeo"
    ) %>%
    addDrawToolbar(
      targetLayer = "randgeo",
      editOptions = editToolbarOptions()
    ) %>%
    fitBounds(48,48,62,62)

  library(mapedit)
  editMap(lf)
}
