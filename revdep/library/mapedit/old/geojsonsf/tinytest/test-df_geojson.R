

#"data.frame converted to GeoJSON", {

	n <- 1
	df <- data.frame(lon = c(1:n, NA), lat = c(1:n, NA) )
	geo <- df_geojson( df, lon = "lon", lat = "lat")
	expect_true(length(geo) == 2)
	expect_true(geo[1] == '{"type":"Point","coordinates":[1.0,1.0]}')
	expect_true(geo[2] == 'null')
	expect_true(all(sapply(geo, jsonify::validate_json)))

	df <- data.frame(lon = c(1:n, NA), lat = c(1:n, NA), id = 1:(n+1), val = letters[1:(n+1)])
	geo <- df_geojson( df, lon = "lon", lat = "lat")
	expect_true(length(geo) == 1)
	expect_true(jsonify::validate_json( geo ) )
	geo <- df_geojson( df, lon = "lon", lat = "lat", atomise = T)
	expect_true(all(sapply(geo, jsonify::validate_json)))


##"z and m handled", {

	n <- 1
	df <- data.frame(lon = c(1:n, NA), lat = c(1:n, NA), z = c(1:n, NA) )
	geo <- df_geojson( df, lon = "lon", lat = "lat", z = "z")
	expect_true(length(geo) == 2)
	expect_true(geo[1] == '{"type":"Point","coordinates":[1.0,1.0,1.0]}')
	expect_true(geo[2] == 'null')
	expect_true(all(sapply(geo, jsonify::validate_json)))

	df <- data.frame(lon = c(1:n, NA), lat = c(1:n, NA), z = c(1:n, NA), m = c(1:n, NA), id = 1:(n+1), val = letters[1:(n+1)])
	geo <- df_geojson( df, lon = "lon", lat = "lat", z = "z", m = "m")
	expect_true(length(geo) == 1)
	expect_true(jsonify::validate_json( geo ) )
	geo <- df_geojson( df, lon = "lon", lat = "lat", atomise = T)
	expect_true(all(sapply(geo, jsonify::validate_json)))

	expect_error(
		df_geojson( df, lon = "lon", lat = "lat", m = "m")
		, "z must be supplied when using m"
	)

##"Factors are handled", {

	df <- data.frame(id = letters[1:2], lat = c(0,1), lon = c(1,0), stringsAsFactors = TRUE )
	geo <- df_geojson(df, lat = "lat", lon = "lon")
	expect_equal(as.character(geo), '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":"a"},"geometry":{"type":"Point","coordinates":[1.0,0.0]}},{"type":"Feature","properties":{"id":"b"},"geometry":{"type":"Point","coordinates":[0.0,1.0]}}]}')

	geo <- df_geojson(df, lat = "lat", lon = "lon", factors_as_string = FALSE)
	expect_equal(as.character(geo), '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[1.0,0.0]}},{"type":"Feature","properties":{"id":2},"geometry":{"type":"Point","coordinates":[0.0,1.0]}}]}')
