#' @keywords internal
geojson_to_sf = function(x) {
  do.call(
    rbind,
    lapply(x, function(x) {
      x <- lapply(x, fix_geojson_coords)
      sf::read_sf(
        jsonlite::toJSON(x, force=TRUE, auto_unbox=TRUE, digits = 6)
      )
    })
  )
}

#' @keywords internal
st_as_sfc.geo_list = function(x, crs = 4326, ...) {
  geom_sf = sf::read_sf(
    jsonlite::toJSON(x, auto_unbox=TRUE, force=TRUE, digits = 6)
  )
  suppressWarnings({
    sf::st_crs(geom_sf) = crs
  })
  return(geom_sf)
}

#' @keywords internal
st_as_sf.geo_list = function(x, crs = 4326, ...) {
  if(x$type != "Feature") {
    stop("should be of type 'Feature'", call.=FALSE)
  }

  geom_sf <- st_as_sfc.geo_list(x)
  suppressWarnings({
    sf::st_crs(geom_sf) = crs
  })
  return(geom_sf)
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
    ft$geometry$coordinates <- lapply(
      ft$geometry$coordinates,
      function(coords) {
        matrix(
          unlist(ft$geometry$coordinates),
          ncol = 2,
          byrow = TRUE
        )
      }
    )
  }

  ft
}

#' @keywords internal
combine_list_of_sf <- function(sf_list, crs = sf::st_crs(sf_list[[1]])) {
  if(length(sf_list) == 0) {return(NULL)}
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
    geometry = sf::st_sfc(
      unlist(lapply(sf_list, function(x) sf::st_geometry(x)), recursive=FALSE)
    ),
    crs = sf::st_crs(crs)
  )
}
