
##"r types are converted correctly", {

	# character, integer, numeric, logical, posixct, posixlt, date
	js <- '{"type":"Point","coordinates":[0,0]}'
	sf <- geojson_sf(js)
	sf$test <- as.Date("2018-01-01")
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":\"2018-01-01\"},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == "2018-01-01")

	sf$test <- as.POSIXct("2018-01-01 01:01:59")
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":\"2018-01-01 01:01:59\"},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == "2018-01-01 01:01:59")

	sf$test <- TRUE
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":true},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == TRUE)

	sf$test <- "a"
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":\"a\"},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == "a")

	sf$test <- 1L
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":1},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == 1.0) ## ints vs numerics not yet implemented

	sf$test <- 1.0
	j <- sf_geojson(sf)
	expect_true(jsonify::validate_json(j))
	expect_equal(
		as.character( j ),
		"{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"test\":1.0},\"geometry\":{\"type\":\"Point\",\"coordinates\":[0.0,0.0]}}]}"
	)
	sf2 <- geojson_sf( j )
	expect_true(sf2$test == 1.0)

