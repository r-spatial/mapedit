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

# now try to add in properties
#  this does not seem critical at this point
#  but will be necessary to be considered complete
props <- lapply(me_gj, function(x) do.call(data.frame, list(x$properties, stringsAsFactors=FALSE)))
sf:::cbind.sf(me_sf, do.call(rbind,props))

