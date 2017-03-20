# test geojson conversion functions
library(sf)
st_as_sfc.geo_list = function(x, ...) {
  x = switch(x$type,
             Point = st_point(x$coordinates),
             MultiPoint = st_multipoint(x$coordinates),
             LineString = st_linestring(x$coordinates),
             MultiLineString = st_multilinestring(x$coordinates),
             Polygon = st_polygon(x$coordinates),
             MultiPolygon = st_multipolygon(x$coordinates),
             GeometryCollection = st_geometrycollection(
               lapply(x$geometries, function(y) st_as_sfc.geo_list(y)[[1]])),
             stop("unknown class")
  )
  st_sfc(x, crs = st_crs(4326))
}

st_as_sf.geo_list = function(x, ...) {
  if(x$type != "Feature") stop("should be of type 'Feature'", call.=FALSE)

  x <- fix_geojson_coords(x)

  props <- do.call(
    data.frame,
    modifyList(
      Filter(Negate(is.null), x$properties),
      list(stringsAsFactors=FALSE)
    )
  )

  geom_sf <- st_as_sfc.geo_list(x$geometry)
  st_sf(props, feature=geom_sf)
}


fix_geojson_coords <- function(ft) {

  if(ft$geometry$type == "Point") {
    ft$geometry$coordinates <- unlist(ft$geometry$coordinates)
  }

  if(ft$geometry$type == "LineString") {
    ft$geometry$coordinates <- matrix(
      unlist(ft$geometry$coordinates),
      ncol = 2,
      byrow = TRUE
    )
  }

  if(!(ft$geometry$type %in% c("Point", "LineString"))) {
    ft$geometry$coordinates <- list(
      matrix(
        unlist(ft$geometry$coordinates),
        ncol = 2,
        byrow = TRUE
      )
    )
  }

  ft
}

convert_geojson_coords <- function(gj) {
  feats <- lapply(
    gj,
    function(ft) {
      ft <- fix_geojson_coords(ft)
      st_as_sfc.geo_list(ft$geometry)
    }
  )

  st_sf(
    features = do.call(st_sfc, unlist(feats, recursive=FALSE))
  )
}
