
## "Geometry object has correct members", {

	## no 'coordinates' array
	js <- '{"type":"Point","coordinate":[0,0]}'
	expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'coordinates' member at object index 0 - invalid GeoJSON")

	## no 'type' key
	js <- '{"geometry":"Point","coordinates":[0,0]}'
	expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'type' member at object index 0 - invalid GeoJSON")

	js <- '{"type":"Point"}'
	expect_error(geojson_sf(js), "No 'coordinates' member at object index 0 - invalid GeoJSON")

	js <- '{"type":"Point","coordinates":null}'
	expect_error(geojson_sf(js), "No 'array' member at object index 0 - invalid GeoJSON")

	js <- '{"type":"Point","coordinates":[]}'
	expect_error(geojson_sf(js), "Invalid lon/lat object")

## "Feature Object has correct members", {

	## type : Feature
	## MUST HAVE
	## - geometry : {}
	## - properties : {}

	## invalid 'geometry' member
	js <- '{"type" : "Feature","properties" : {},"geom" : {"type" : "Point","coordinates" : [ 0, 0]}}'
	expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'geometry' member at object index 0 - invalid GeoJSON")

	## invalid 'properties' member
	js <- '{"type" : "Feature","property" : {},"geometry" : {"type" : "Point","coordinates" : [ 0, 0]}}'
	# expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'properties' member at object index 0 - invalid GeoJSON")
	expect_true(!"property" %in% names(geojson_sf(js)))

	js <- '[{"type" : "Feature","properties" : {},"geometry" : {"type" : "Point","coordinates" : [ 0, 0]}},
  {"type" : "Feature","property" : {},"geometry" : {"type" : "Point","coordinates" : [ 0, 0]}}]'
	# expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'properties' member at object index 1 - invalid GeoJSON")
	expect_true( ncol( geojson_sf(js) ) == 1 )

	js <- '[{"type" : "Feature","properties" : {"id":5},"geometry" : {"type" : "Point","coordinates" : [ 0, 0]}},
  {"type" : "Feature","property" : {},"geometry" : {"type" : "Point","coordinates" : [ 0, 0]}}]'
	# expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'properties' member at object index 1 - invalid GeoJSON")
	expect_true("id" %in% names( geojson_sf( js ) ) )
  expect_true(sum(geojson_sf(js)$id, na.rm = T) == 5)

## "Featurecollection has correct members", {

  ## Feature Colection
	## MUST HAVE
	## - features

	## invalid 'feature'
	js <- '{"type": "FeatureCollection","feature": [
  {"type": "Feature","properties": null,"geometry": {"type": "Point", "coordinates": [100.0, 0.0]}},
  {"type": "Feature","properties": null,"geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}},
  {"type": "Feature","properties": null,"geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}}]}'
	expect_error(geojsonsf:::rcpp_geojson_to_sf(js, F),"No 'features' member at object index 0 - invalid GeoJSON")




