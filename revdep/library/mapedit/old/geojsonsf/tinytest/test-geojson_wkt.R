## "wkt created correctly", {
	p <- '{"type":"Point", "coordinates":[0,0]}'
	mp <- '{"type":"MultiPoint", "coordinates":[[0,0],[2.324,2]]}'
	ls <- '{"type":"LineString", "coordinates":[[0,0],[1,1]]}'
	ml <- '{"type":"MultiLineString","coordinates":[[[100.0,0.0],[101.0,1.0]],[[102.0,2.0],[103.0,3.0]]]}'
	poly <- '{"type":"Polygon","coordinates":[[[180.0,40.0],[180.0,50.0],[170.0,50.0],[170.0,40.0],[180.0,40.0]]]}'
	mpoly <- '{"type":"MultiPolygon","coordinates":[[[[180,40],[180,50],[170,50],[170,40],[180,40]],[[0,0],[1,1],[2,2],[2,0]]],[[[-170,40],[-170,50],[-180,50],[-180,40],[-170,40]]]]}'
	gc <- '{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}'
	f <- '{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}}'
	fc <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}}]}'
	fcgc <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}}]}'

	p <- geojson_wkt(p)
	mp <- geojson_wkt(mp)
	ls <- geojson_wkt(ls)
	ml <- geojson_wkt(ml)
	poly <- geojson_wkt(poly)
	mpoly <- geojson_wkt(mpoly)
	gc <- geojson_wkt(gc)
	f <- geojson_wkt(f)
	fc <- geojson_wkt(fc)
	fcgc <- geojson_wkt(fcgc)

	expect_true(class(p) == "data.frame")
	expect_true(class(mp) == "data.frame")
	expect_true(class(ml) == "data.frame")
	expect_true(class(poly) == "data.frame")
	expect_true(class(mpoly) == "data.frame")
	expect_true(class(gc) == "data.frame")
	expect_true(class(f) == "data.frame")
	expect_true(class(fc) == "data.frame")
	expect_true(class(fcgc) == "data.frame")

##"WKT with NULL objects", {

	js <- '{"type":"Feature","properties":{"id":1.0},"geometry":null}'
	expect_true( geojson_wkt( js )$geometry[[1]] == "POINT EMPTY" )

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0, "val":"a"},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
	{"type":"Feature","properties":{"id":2.0, "val":"b"},"geometry":null}]}'

	df <- geojson_wkt( js )

	expect_true( nrow(df) == 2 )
	expect_true( attr( df$geometry[[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( df$geometry[[2]], "class" )[[1]] == "wkt" )

## "WKT with Z and M dimensions handled",{

	p <- '{"type":"Point", "coordinates":[0,0,0]}'
	mp <- '{"type":"MultiPoint", "coordinates":[[0,0,0,0],[2.324,2,1,1]]}'
	ls <- '{"type":"LineString", "coordinates":[[0,0,0,0],[1,1,1,1]]}'
	ml <- '{"type":"MultiLineString","coordinates":[[[100.0,0.0],[101.0,1.0]],[[102.0,2.0],[103.0,3.0]]]}'
	poly <- '{"type":"Polygon","coordinates":[[[180.0,40.0],[180.0,50.0],[170.0,50.0],[170.0,40.0],[180.0,40.0]]]}'
	mpoly <- '{"type":"MultiPolygon","coordinates":[[[[180,40],[180,50],[170,50],[170,40],[180,40]],[[0,0],[1,1],[2,2],[2,0]]],[[[-170,40],[-170,50],[-180,50],[-180,40],[-170,40]]]]}'
	f <- '{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}}'
	gc <- '{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}'
	fc <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}}]}'
	fcgc <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}}]}'

	p <- geojson_wkt( p )
	mp <- geojson_wkt( mp )
	ls <- geojson_wkt( ls )
	ml <- geojson_wkt(ml)
	poly <- geojson_wkt(poly)
	mpoly <- geojson_wkt(mpoly)
	f <- geojson_wkt(f)
	gc <- geojson_wkt(gc)
	fc <- geojson_wkt(fc)
	fcgc <- geojson_wkt( fcgc )

	expect_equal( attr(p$geometry[[1]], "class")[[2]], "XYZ" )
	expect_equal( attr(mp$geometry[[1]], "class")[[2]], "XYZM" )
	expect_equal( attr(ls$geometry[[1]], "class")[[2]], "XYZM" )
	expect_equal( attr(ml$geometry[[1]], "class")[[2]], "XY" )
	expect_equal( attr(poly$geometry[[1]], "class")[[2]], "XY" )
	expect_equal( attr(f$geometry[[1]], "class")[[2]], "XY" )
	expect_equal( attr(gc$geometry[[1]], "class")[[2]], "XY" )
	expect_equal( attr(fc$geometry[[1]], "class")[[2]], "XY" )
	expect_equal( attr(fcgc$geometry[[1]], "class")[[2]], "XY" )

	## test all are one-list level deep
	expect_true( attr( p[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( mp[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( ls[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( ml[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( poly[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( mpoly[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( gc[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( f[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( fc[1, "geometry"][[1]], "class" )[[1]] == "wkt" )
	expect_true( attr( fcgc[1, "geometry"][[1]], "class" )[[1]] == "wkt" )

	# ## TODO
	# ## sf::st_as_sf( data.frame( geom = p[1, "geometry"] ), wkt = "geom" )
	# make_df <- function( x ) {
	# 	df <- data.frame(
	# 		geom = as.character( x[1, "geometry"][[1]] )
	# 	)
	# 	return(df)
	# }
	#
	# sf::st_as_sf( make_df( p ), wkt = "geom" )
	# sf::st_as_sf( make_df( mp ), wkt = "geom" )
	# sf::st_as_sf( make_df( ls ), wkt = "geom" )
	# sf::st_as_sf( make_df( ml ), wkt = "geom" )
	# sf::st_as_sf( make_df( poly ), wkt = "geom" )
	# sf::st_as_sf( make_df( mpoly ), wkt = "geom" )
	# sf::st_as_sf( make_df( gc ), wkt = "geom" )
	# sf::st_as_sf( make_df( f ), wkt = "geom" )
	# sf::st_as_sf( make_df( fc ), wkt = "geom" )
	# sf::st_as_sf( make_df( fcgc ), wkt = "geom" )


## "mixed dimensions not allowed",{

	mp <- '{"type":"MultiPoint", "coordinates":[[0,0,0],[2.324,2,1,1]]}'
	ls <- '{"type":"LineString", "coordinates":[[0,0,0,0],[1,1,1]]}'
	ml <- '{"type":"MultiLineString","coordinates":[[[100.0,0.0, 1],[101.0,1.0]],[[102.0,2.0],[103.0,3.0]]]}'
	poly <- '{"type":"Polygon","coordinates":[[[180.0,40.0],[180.0,50.0, 2],[170.0,50.0],[170.0,40.0],[180.0,40.0]]]}'
	mpoly <- '{"type":"MultiPolygon","coordinates":[[[[180,40],[180,50],[170,50],[170,40],[180,40]],[[0,0],[1,1],[2,2],[2,0]]],[[[-170,40],[-170,50],[-180,50,3],[-180,40],[-170,40]]]]}'
	fcgc <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2,3]]}]}}]}'

	expect_error( geojson_wkt( mp ), "geojsonsf - different coordinate dimensions found" )
	expect_error( geojson_wkt( ls ), "geojsonsf - different coordinate dimensions found" )
	expect_error( geojson_wkt(ml), "geojsonsf - different coordinate dimensions found" )
	expect_error( geojson_wkt(poly), "geojsonsf - different coordinate dimensions found" )
	expect_error( geojson_wkt(mpoly), "geojsonsf - different coordinate dimensions found" )
	expect_error( geojson_wkt(fcgc), "geojsonsf - different coordinate dimensions found" )

