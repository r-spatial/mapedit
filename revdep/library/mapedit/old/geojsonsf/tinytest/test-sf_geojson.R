## "sfc objects converted to GeoJSON", {

	js <- '{"type":"Point","coordinates":[0.0,0.0]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j) )

	js <- '{"type":"LineString","coordinates":[[0.0,0.0],[1.0,1.0]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j) )

	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]],[[2.0,2.0],[3.0,3.0]]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"Polygon","coordinates":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j) )

	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[0.5,0.5],[0.5,0.75],[0.75,0.75],[0.75,0.5],[0.5,0.5]]],[[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
	sf <- geojson_sfc( js )
	j <- sfc_geojson(sf)
	expect_true( j == js )
	expect_true( jsonify::validate_json( j ) )

## "sf objects converted to GeoJSON", {

	js <- '{"type":"Point","coordinates":[0.0,0.0]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"LineString","coordinates":[[0.0,0.0],[1.0,1.0]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiLineString","coordinates":[[[0.0,0.0],[1.0,1.0]],[[2.0,2.0],[3.0,3.0]]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"Polygon","coordinates":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j) )

	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( j == js)
	expect_true( jsonify::validate_json( j ) )

	js <- '{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]],[[0.5,0.5],[0.5,0.75],[0.75,0.75],[0.75,0.5],[0.5,0.5]]],[[[2.0,2.0],[2.0,3.0],[3.0,3.0],[3.0,2.0],[2.0,2.0]]]]}'
	sf <- geojson_sf( js )
	j <- sf_geojson(sf)
	expect_true( j == js )
	expect_true( jsonify::validate_json( j ) )

## "features with null geometries handled correctly", {
	js <- '{"type":"Feature","properties":{},"geometry":null}'
	sf <- geojson_sf( js )
	expect_equal( attr( sf$geometry[[1]], "class")[1], "XY" )

## "GeometryCollections correctly closed", {
	js <- '{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100.0,0.0]},{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]},{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0],[2.0,2.0]]}]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( jsonify::validate_json( j ) )
	expect_true(js == j)

	js <- '{"type":"GeometryCollection","geometries":[{"type":"MultiLineString","coordinates":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}]}'
	sf <- geojson_sfc(js)
	j <- sfc_geojson(sf)
	expect_true( jsonify::validate_json(j))
	sf2 <- geojson_sfc(j)
	expect_equal(sf, sf2)

	js <- '{"type":"GeometryCollection","geometries":[{"type":"MultiLineString","coordinates":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( jsonify::validate_json(j))
	sf2 <- geojson_sf(j)
	expect_equal(sf, sf2)

	js <- '{"type":"GeometryCollection","geometries":[{"type":"Polygon","coordinates":[[[0,0],[0,1],[1,1],[1,0],[0,0]]]}]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( jsonify::validate_json(j))
	sf2 <- geojson_sf(j)
	expect_equal(sf, sf2)

	js <- '{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[0,0],[0,1],[1,1],[1,0],[0,0]]]]}]}'
	sf <- geojson_sf(js)
	j <- sf_geojson(sf)
	expect_true( jsonify::validate_json(j))
	sf2 <- geojson_sf(j)
	expect_equal(sf, sf2)


## "sf without properties not converted to FeatureCollections", {
	js <- '[{"type":"Polygon","coordinates":[[[0,0],[1,1]]]},{"type":"MultiLineString","coordinates":[[[0,0],[1,1]],[[3,3],[4,4]]]}]'
	sf <- geojson_sf(js)
	j1 <- sf_geojson(sf, atomise = F)
	j2 <- sf_geojson(sf, atomise = T)
	expect_true(all( j1 == j2 ))
	expect_true( all( jsonify::validate_json( j1 ) ) )
	expect_true( all( jsonify::validate_json( j1 ) ) )


	js <- '[{"type":"Polygon","coordinates":[[[0,0],[1,1]]]},{"type":"MultiLineString","coordinates":[[[0,0],[1,1]]]},{"type":"GeometryCollection","geometries":[{"type":"Point", "coordinates":[100.0, 0.0]},{"type":"LineString","coordinates":[[101,0],[102,1]]},{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}]'
	sf <- geojson_sf(js)
	j1 <- sf_geojson(sf, atomise = F)
	j2 <- sf_geojson(sf, atomise = T)
	expect_true(all( j1 == j2 ))
	expect_true( all( jsonify::validate_json( j1 ) ) )
	expect_true( all( jsonify::validate_json( j1 ) ) )

## "sf with properties converted to FeatureCollection", {

	js <- '{"type":"Feature","properties":{"prop0":"value0"},"geometry":{"type":"LineString","coordinates":[[100.0,0.0],[101.0,1.0]]}}'
	sf <- geojson_sf(js)
	expect_true( sf_geojson( sf, atomise = T ) == gsub(" |\\r|\\n|\\t","",js))

	fgc <- '{"type":"Feature","geometry":{"type":"GeometryCollection","geometries": [{"type":"Point","coordinates":[100.0,0.0]},{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]}]},"properties":{"prop0":"value0","prop1":"value1"}}'
	sf <- geojson_sf(fgc)
	expect_true(grepl("prop0",sf_geojson(sf)))
	expect_true(grepl("prop1",sf_geojson(sf)))
	expect_true(grepl("FeatureCollection",sf_geojson(sf)))

	js <- '{"type" : "Feature","properties" : {},"geometry" : {"type": "GeometryCollection", "geometries": [{"type":"Point","coordinates":[100.0,0.0]},{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]},{"type":"MultiPoint","coordinates":[[0.0,0.0],[1.0,1.0],[2.0,2.0]]}]}}'
	sf <- geojson_sf(js)
	## properties are null, so they wont' be found to be converted back to JSON
	expect_false(grepl("properties",sf_geojson(sf)))

	fc1 <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":null,"geometry":{"type":"Point","coordinates":[100.0,0.0]}}]}'
	sf <- geojson_sf(fc1)
  expect_false(grepl("properties",sf_geojson(sf)))

  js <- '{"type": "FeatureCollection","features":[{"type": "Feature","id": "id0","geometry": {"type": "LineString","coordinates":[[102,0],[103,1],[104,0],[105,1]]},"properties": {"prop0":"value0","prop1":"value1"}},{"type": "Feature","id": "id1","geometry":{"type":"Polygon","coordinates":[[[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0],[100.0, 0.0]]]},"properties":{"prop0":"value0","prop1":"value1"}}]}'

  sf <- geojson_sf(js)
  expect_true(grepl("properties",sf_geojson(sf)))
  j <- sf_geojson(sf)
  expect_true( jsonify::validate_json(j))
  expect_true(length(sf_geojson(sf, atomise = T)) == 2)
  expect_true( jsonify::validate_json( sf_geojson(sf, atomise = T)[1]))
  expect_true( jsonify::validate_json( sf_geojson(sf, atomise = T)[2]))

##"sf object with properties converted to sfc", {
	fgc <- '{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100,0]},{"type":"LineString","coordinates":[[101,0],[102,1]]}]},"properties": {"prop0": "value0","prop1": "value1"}}'
	sf <- geojson_sf(fgc)
	expect_false(grepl("properties", sfc_geojson( sf$geometry )))

## "errors are handled", {
	js <- '{"type":"Point","coordinates":[0.0,0.0]}'
	sf <- geojson_sf(js)
	expect_error(sfc_geojson(sf),"Expected an sfc object")

	js <- '{"type":"Point","coordinates":[0,0]}'
	sf <- geojson_sfc(js)
	expect_error(sf_geojson(sf),"Expected an sf object")


## "factors are numeric", {

	fgc <- '{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100.0,0.0]},{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]}]},"properties":{"prop0":"value0","prop1":"value1"}}'
	sf <- geojson_sf(fgc)
	sf$prop0 <- as.factor(sf$prop0)
	geo <- sf_geojson(sf, factors_as_string = FALSE )
	expect_false(grepl("value0", geo))

	geo <- sf_geojson(sf, factors_as_string = TRUE )
	expect_true(grepl("value0", geo))


## "factor levels handled", {

	geo <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":"1"},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},{"type":"Feature","properties":{"id":"1"},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}]}'
	sf <- geojson_sf( geo )
	sf$id <- as.factor( sf$id )

	expect_true( sf_geojson( sf ) == geo )

