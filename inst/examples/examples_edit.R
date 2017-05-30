\dontrun{
  # demonstrate Leaflet.Draw on a layer
  library(sf)
  library(mapview)
  library(leaflet.extras)
  library(mapedit)

  # ?sf::sf
  pol = st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
  mapview(pol) %>%
    editMap(targetLayerId = "pol")

  mapview(franconia[1:2,]) %>%
    editMap(targetLayerId = "franconia[1:2, ]")
}
