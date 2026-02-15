## "geometries are parsed correctly", {

	js <- '{"type": "Point", "coordinates": [101.0, 1.0] }'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_POINT", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type":"MultiPoint","coordinates":[[100.0, 0.0],[101.0, 1.0]]}'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_MULTIPOINT", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "LineString","coordinates":[[100.0, 0.0],[101.0, 1.0]]}'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_LINESTRING", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "MultiLineString","coordinates":[[[100.0, 0.0],[101.0,1.0]],[[102.0,2.0],[103.0,3.0]]]}'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_MULTILINESTRING", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "Polygon","coordinates":[[[180.0,40.0],[180.0,50.0],[170.0,50.0],[170.0,40.0],[180.0,40.0]]]}'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_POLYGON", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "MultiPolygon","coordinates": [[[[180.0, 40.0], [180.0, 50.0], [170.0, 50.0],[170.0, 40.0], [180.0, 40.0]]],[[[-170.0, 40.0], [-170.0, 50.0], [-180.0, 50.0],[-180.0, 40.0], [-170.0, 40.0]]]]}'
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_MULTIPOLYGON", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "GeometryCollection",
		"geometries": [
			{"type": "Point", "coordinates": [100.0, 0.0]},
			{"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
			{"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]}'

	sfc <- geojson_sfc(js)
	sf <- geojson_sf(js)
	expect_true(all(c("sfc_GEOMETRY", "sfc") %in% attr(sf$geometry, "class")))
	expect_true(jsonify::validate_json(sf_geojson(sf)))

	js <- '{"type": "GeometryCollection",
	  "geometry": [
	    {"type": "Point", "coordinates": [100.0, 0.0]},
	    {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
	    {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]}'

	expect_error(geojson_sf(js),"No 'geometries' member at object index 0 - invalid GeoJSON")
	expect_error(geojson_wkt(js),"No 'geometries' member at object index 0 - invalid GeoJSON")


