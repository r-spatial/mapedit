get_center = function(x) {
  bb_sf = st_bboxx(x)
  sf::st_coordinates(sf::st_centroid(bb_sf))
}

st_bboxx = function(x, sfg = TRUE) {
  bb = sf::st_bbox(x)
  if (sfg) {
    outer = matrix(c(bb[1], bb[2],
                     bb[1], bb[4],
                     bb[3], bb[4],
                     bb[3], bb[2],
                     bb[1], bb[2]),
                   ncol = 2,
                   byrow = TRUE)
    return(sf::st_polygon(list(outer)))
  } else {
    return(bb)
  }
}
