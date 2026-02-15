## "simplify arguemnt simplifies or not", {

	js <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"LineString","coordinates":[[0.0,0.0],[1.0,1.0],[2.0,1.0]]}},{"type":"Feature","properties":{},"geometry":{"type":"MultiLineString","coordinates":[[[2.0,2.0],[1.0,3.0]],[[0.0,0.0],[1.0,1.0],[2.0,1.0]]]}}]}'
	sf <- geojson_sf( js )
	geo_simple <- sf_geojson( sf )
	expect_true(length(geo_simple) == 2)
	geo_feature <- sf_geojson( sf, simplify = FALSE )
	expect_true(length(geo_feature) == 1)

	expect_true(jsonify::validate_json(geo_simple[1]))
	expect_true(jsonify::validate_json(geo_simple[2]))
	expect_true(jsonify::validate_json(geo_feature))

