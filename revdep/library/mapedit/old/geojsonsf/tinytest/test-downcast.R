# context("downcast")
#
# library(sf)
#
# pt1 <- sf::st_sf( geometry = sf::st_sfc( sf::st_point(c(0,0))))
# mp1 <- sf::st_sf( geometry = sf::st_sfc( sf::st_multipoint(x = matrix(1:6, ncol = 2))))
# mp2 <- sf::st_sf( geometry = sf::st_sfc( sf::st_multipoint(x = matrix(1:6, ncol = 3))))
# mp3 <- sf::st_sf( geometry = sf::st_sfc( sf::st_multipoint(x = matrix(1:8, ncol = 4))))
# pt2 <- sf::st_sf( geometry = sf::st_sfc( sf::st_point(c(1,1,3,4))))
# ls1 <- sf::st_sf( geometry = sf::st_sfc( sf::st_linestring(x = matrix(1:6, ncol = 2))))
# ls2 <- sf::st_sf( geometry = sf::st_sfc( sf::st_linestring(x = matrix(1:6, ncol = 3))))
# mls1 <- sf::st_sf( geometry = sf::st_sfc( sf::st_multilinestring(x = list(matrix(1:6, ncol = 2), matrix(6:1, ncol = 2), matrix(1:6, ncol = 2)))))
#
# test_that("MULTI objects are down-cast", {
#
# 	js <- '{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0]]}'
# 	sf <- geojson_sf(js)
# 	geo <- geojsonsf::sf_geojson( sf )
#
#
# 	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]]]}'
# 	sf <- geojson_sf(js)
#
#
# 	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]],[[2.0,2.0],[3.0,3.0]]]}'
# 	sf <- geojson_sf(js)
#
#
# 	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
# 	sf <- geojson_sf(js)
#
#
# 	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[0.5,0.5],[0.5,0.75],[0.75,0.75],[0.75,0.5],[0.5,0.5]]],[[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
# 	sf <- geojson_sf( js )
#
#
# })
