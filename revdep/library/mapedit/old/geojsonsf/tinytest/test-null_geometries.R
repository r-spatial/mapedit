## "GeoJSON with null geometries parsed correctly", {

	js <- '{"type":"Feature","properties":{"id":1.0},"geometry":null}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_POINT")
	js2 <- sf_geojson(sf, atomise = T)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[1, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_POINT")
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0]]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_MULTIPOINT")
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"LineString","coordinates":[[0.0,0.0],[1.0,1.0]]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_LINESTRING")
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]]]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_MULTILINESTRING")
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":2.0},"geometry":null},
  {"type":"Feature","properties":{"id":1.0},"geometry":{"type":"Polygon","coordinates":[[[0.0,0.0],[1.0,1.0],[2.0,2.0],[0.0,0.0]]]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 2)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_GEOMETRY") ## because the first null starts as a point
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[1, 'geometry'] == "POINT EMPTY")
	expect_true(wkt[3, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
	{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[1.0,1.0],[2.0,2.0],[0.0,0.0]]]]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	expect_true(attr(sf$geometry, "n_empty") == 1)
	expect_true(attributes(sf$geometry)[['class']][1] == "sfc_MULTIPOLYGON")
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")

	js <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"id":1.0},"geometry":{"type":"GeometryCollection","geometries": [
	{"type": "Point","coordinates": [100.0,0.0]},
	{"type": "LineString","coordinates": [[101.0,0.0],[102.0,1.0]]}]}},
	{"type":"Feature","properties":{"id":2.0},"geometry":null}]}'
	sf <- geojson_sf(js)
	wkt <- geojson_wkt(js)
	expect_true(wkt[2, 'geometry'] == "POINT EMPTY")
	expect_true(attr(sf$geometry, "n_empty") == 1)
	js2 <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js2))
	expect_true(gsub(" |\\r|\\n|\\t","",js) == js2)

	js <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1.0},"geometry":{"type":"GeometryCollection","geometries": [{"type": null},{"type": "LineString","coordinates": [[101.0,0.0],[102.0,1.0]]}]}}]}'
	expect_error(geojson_sf(js), "No 'type' member at object index 0 - invalid GeoJSON")
	expect_error(geojson_wkt(js), "No 'type' member at object index 0 - invalid GeoJSON")

## "null sf objects converted to GeoJSON", {

	## GEOMETRYCOLLECTION with null entries - nul entries are dropped
	sf <- structure(list(geometry = structure(list(structure(list(structure(c(0, 0),
  class = c("XY", "POINT", "sfg")), structure(c(1, 2, 3, 4), .Dim = c(2L,
	2L), class = c("XY", "LINESTRING", "sfg")), structure(c(NA_real_,
	NA_real_), class = c("XY", "POINT", "sfg")), structure(list(), class = c("XY",
	"MULTIPOLYGON", "sfg"))), class = c("XY", "GEOMETRYCOLLECTION",
	"sfg"))), class = c("sfc_GEOMETRYCOLLECTION", "sfc"), precision = 0, bbox = structure(c(xmin = 0,
	ymin = 0, xmax = 2, ymax = 4), class = "bbox"), crs = structure(list(
	epsg = NA_integer_, proj4string = NA_character_), class = "crs"), n_empty = 0L)), row.names = 1L, class = c("sf",
	"data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c("constant",
	"aggregate", "identity"), .Names = character(0)))

	js <- sf_geojson(sf)
	expect_true(jsonify::validate_json(js))
	expect_false(grepl("null", js))

##  "null objects", {

	js <- '{"type":"Feature","properties":{},"geometry":null}'
	sf <- geojson_sf( js )
	js2 <- sf_geojson( sf, simplify = F )
	expect_true(jsonify::validate_json(js2))

	js <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":null}]}'
	sf <- geojson_sf( js )
	js2 <- sf_geojson( sf, simplify = T )
	expect_true(jsonify::validate_json(js2))
	js2 <- sf_geojson( sf, simplify = F )
	expect_true(js2 == js)
	expect_true(jsonify::validate_json(js2))

	# sf <- structure(list(structure(c(NA_real_, NA_real_), class = c("XY",
	# 	"POINT", "sfg"))), class = c("sfc_POINT", "sfc"), precision = 0, bbox = structure(c(xmin = NA_real_,
	# 	ymin = NA_real_, xmax = NA_real_, ymax = NA_real_), crs = structure(list(
	# 	epsg = NA_integer_, proj4string = NA_character_), class = "crs"), class = "bbox"), crs = structure(list(
	# 	epsg = NA_integer_, proj4string = NA_character_), class = "crs"), n_empty = 1L)
	#
	# sfc_geojson( sf )

	sf <- structure(list(geometry = structure(list(structure(c(NA_real_,
		NA_real_), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT",
		"sfc"), precision = 0, bbox = structure(c(xmin = NA_real_, ymin = NA_real_,
		xmax = NA_real_, ymax = NA_real_), crs = structure(list(epsg = NA_integer_,
		proj4string = NA_character_), class = "crs"), class = "bbox"), crs = structure(list(
		epsg = NA_integer_, proj4string = NA_character_), class = "crs"), n_empty = 1L)), row.names = 1L, class = c("sf",
		"data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c("constant",
		"aggregate", "identity"), .Names = character(0)))

	expect_equal( as.character(sf_geojson( sf )), "null")
	js <- sf_geojson( sf , simplify = FALSE )
	expect_true( jsonify::validate_json( js ) )
	expect_true(grepl("null",js))

