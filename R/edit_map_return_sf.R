#' @keywords internal
geojson_to_sf = function(x) {
  do.call(
    rbind,
    lapply(x, function(x) {
      x <- lapply(x, fix_geojso)
      sf::read_sf(
        jsonlite::toJSON(x, force=TRUE, auto_unbox=TRUE)
      )
    })
  )
}

#' @keywords internal
st_as_sfc.geo_list = function(x, ...) {
  sf::read_sf(
    jsonlite::toJSON(x, auto_unbox=TRUE, force=TRUE)
  )
}

#' @keywords internal
st_as_sf.geo_list = function(x, ...) {
  if(x$type != "Feature") {
    stop("should be of type 'Feature'", call.=FALSE)
  }

  x <- fix_geojson_coords(x)

  #props <- do.call(
  #  data.frame,
  #  modifyList(
  #    Filter(Negate(is.null), x$properties),
  #    list(stringsAsFactors=FALSE)
  #  )
  #)

  geom_sf <- st_as_sfc.geo_list(x)
  # if props are empty then we need to handle differently
  #if(nrow(props) == 0 ) {
  #  return(sf::st_sf(feature=geom_sf, crs = sf::st_crs(4326)))
  #} else {
  #  return(sf::st_sf(props, feature=geom_sf, crs = sf::st_crs(4326)))
  #}
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
        dplyr::select_(
          as.data.frame(x, stringsAsFactors=FALSE),
          paste0("-",attr(x, "sf_column", exact=TRUE))
        )
      }
    )
  )

  sf::st_sf(
    props,
    feature = sf::st_sfc(
      unlist(lapply(sf_list, function(x) sf::st_geometry(x)), recursive=FALSE)
    ),
    crs = sf::st_crs(4326)
  )
}
