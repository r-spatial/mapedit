#' @keywords internal
st_as_sfc.geo_list = function(x, ...) {
  x = switch(x$type,
             Point = sf::st_point(x$coordinates),
             MultiPoint = sf::st_multipoint(x$coordinates),
             LineString = sf::st_linestring(x$coordinates),
             MultiLineString = sf::st_multilinestring(x$coordinates),
             Polygon = sf::st_polygon(x$coordinates),
             MultiPolygon = sf::st_multipolygon(x$coordinates),
             GeometryCollection = sf::st_geometrycollection(
               lapply(x$geometries, function(y) st_as_sfc.geo_list(y)[[1]])),
             stop("unknown class")
  )
  sf::st_sfc(x, crs = sf::st_crs(4326))
}

#' @keywords internal
st_as_sf.geo_list = function(x, ...) {
  if(x$type != "Feature") {
    stop("should be of type 'Feature'", call.=FALSE)
  }

  x <- fix_geojson_coords(x)

  props <- do.call(
    data.frame,
    modifyList(
      Filter(Negate(is.null), x$properties),
      list(stringsAsFactors=FALSE)
    )
  )

  geom_sf <- st_as_sfc.geo_list(x$geometry)
  # if props are empty then we need to handle differently
  if(nrow(props) == 0 ) {
    return(sf::st_sf(feature=geom_sf, crs = sf::st_crs(4326)))
  } else {
    return(sf::st_sf(props, feature=geom_sf, crs = sf::st_crs(4326)))
  }
}

#' @keywords internal
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

#' @keywords internal
combine_list_of_sf <- function(sf_list) {
  props <- dplyr::bind_rows(
    lapply(
      sf_list,
      function(x) {
        dplyr::select(
          as.data.frame(x, stringsAsFactors=FALSE),
          -feature
        )
      }
    )
  )

  sf::st_sf(
    props,
    feature = sf::st_sfc(
      unlist(lapply(sf_list, function(x) x$feature), recursive=FALSE)
    ),
    crs = sf::st_crs(4326)
  )
}
