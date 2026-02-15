 ## CRS and proj4string are deprecated

	geo <- '[]'
	warn <- "crs and proj4string are deprecated. Please now use input and wkt"

	expect_warning( geojson_sfc( geo, crs = 123 ), warn)
	expect_warning( geojson_sf( geo, crs = 123), warn )
	expect_warning( geojson_sfc( geo, proj4string = "abc" ), warn )
	expect_warning( geojson_sf( geo, proj4string = "abc" ), warn )
	expect_warning( geojson_sfc( geo, crs = 123, proj4string = "abc" ), warn )
	expect_warning( geojson_sf( geo, crs = 123, proj4string = "abc" ), warn )

##"input and wkt are set",{

	geo <- '[]'

	expect_error( geojson_sfc( geo, input = 123 ), "If supplying a custom input you must also supply wkt")
	# expect_true( attr(res, "crs")[[1]] == "123" )
	# expect_true( is.na( attr(res, "crs")[[2]] ) )

	# res <- geojson_sf( geo, input = 123)
	# expect_true( attr(res$geometry, "crs")[[1]] == 123 )
	# expect_true( is.na( attr(res$geometry, "crs")[[2]] ) )

	expect_error( geojson_sfc( geo, wkt = "abc" ), "If supplying a custom wkt you must also supply input")
	# expect_true( is.na( attr(res, "crs")[[1]] ) )
	# expect_true( attr(res, "crs")[[2]] == "abc" )

	# res <- geojson_sf( geo, wkt = "abc" )
	# expect_true( is.na( attr(res$geometry, "crs")[[1]] ) )
	# expect_true( attr(res$geometry, "crs")[[2]] == "abc" )

	res <- geojson_sfc( geo, input = 123, wkt = "abc" )
	expect_true( attr(res, "crs")[[1]] == 123 )
	expect_true( attr(res, "crs")[[2]] == "abc" )

	res <- geojson_sf( geo, input = 123, wkt = "abc" )
	expect_true( attr(res$geometry, "crs")[[1]] == 123 )
	expect_true( attr(res$geometry, "crs")[[2]] == "abc" )
