library(geojsonio)

file <- system.file("examples", "norway_maple.kml", package = "geojsonio")
nwy_map <- geojson_read(as.location(file), what = "list")
listviewer::jsonedit(nwy_map)

pt <- nwy_map$features[[1]]$geometry
class(pt) <- "geo_list"


p_list <- lapply(list(c(3.2,4), c(3,4.6), c(3.8,4.4)), st_point)
pt_sfc <- st_sfc(p_list)
pt_sf <- st_sf(x = c("a", "b", "c"), pt_sfc)

# use geojsonio to convert sf to geojson
gj_pt_sf <- geojsonio::geojson_list(pt_sf)

# start by trying to convert a single geojson feature to s
feat <- gj_pt_sf$features[[1]]

# manually translate one geojson feature
one_feat <- st_sf(
  feat$properties,
  st_as_sfc.geo_list(feat$geometry)
)


# now try to convert all of the geojson features
feats <- lapply(
  gj_pt_sf$features,
  function(ft) {
    st_sf(
      ft$properties,
      st_as_sfc.geo_list(ft$geometry)
    )
  }
)

do.call(rbind, feats)


nwy_sf <- convert_geojson_coords(nwy_map$features)
plot(nwy_sf)

library(leaflet)
library(mapedit)
# draw something with mapedit::edit_map
#   and try to convert to sf
me_gj <- edit_map(leaflet())$finished
# coordinates from edit_map not in proper form
#  so added a fix function
me_sf <- convert_geojson_coords(
  lapply(me_gj,function(x) list(geometry = x$geometry))
)
plot(me_sf)

# now let's try to build an st_as_sf.geo_list function
#  that we can apply to our edit_map return
#  and other geojson
nwy_sf2 <- lapply(
  nwy_map$features,
  st_as_sf.geo_list
)

me_sf2 <- lapply(
  me_gj,
  st_as_sf.geo_list
)

# ideally we convert these lists into st_sf
#   but we have a problem if all of the features
#   have different columns

# the easiest solution would be to use dplyr
#   but would require a dependency on dplyr
nwy_sf3 <- combine_list_of_sf(nwy_sf2)
leaflet(nwy_sf3) %>% addMarkers(popup=~description) %>% addTiles()

me_sf3 <- combine_list_of_sf(me_sf2)
leaflet(me_sf3) %>% addPolygons(popup=~feature_type) %>% addTiles()


# test with randgeo
randgeo::geo_point(10) %>%
  {.$features} %>%
  lapply(function(x) st_as_sf.geo_list(x)) %>%
  combine_list_of_sf %>%
  leaflet() %>%
    addMarkers() %>%
    addTiles()


# test with randgeo
list(
  randgeo::geo_point(10),
  randgeo::geo_polygon(10)
) %>%
  {lapply(., function(x) x$features)} %>%
  unlist(recursive = FALSE) %>%
  lapply(function(x) st_as_sf.geo_list(x)) %>%
  combine_list_of_sf() %>%
  plot()
