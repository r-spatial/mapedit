library(sp)
library(sf)
library(leaflet)
library(albersusa)
library(mapedit)
library(dplyr)

usa_sf <- usa_composite() %>% st_as_sf()
borders <- usa_sf %>%
  filter(usa_sf$iso_3166_2 %in% c("AZ","CA")) %>%
  leaflet() %>%
  addPolygons() %>%
  editMap()

plot(borders$finished)
