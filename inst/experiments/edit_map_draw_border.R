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
  edit_map()

border_sf <- borders$finished[[1]]$geometry$coordinates %>%
  unlist() %>%
  matrix(ncol=2, byrow=TRUE) %>%
  st_linestring()

