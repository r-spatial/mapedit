##"empty features are allowed", {

	geo <- '{"type":"FeatureCollection","features":[]}'
	sf <- geojsonsf::geojson_sf( geo )
	expect_true( nrow(sf) == 0)
	expect_true( "sfc_GEOMETRY" %in% attr( sf$geometry , "class" ) )

	## round-trip
	geo2 <- geojsonsf::sf_geojson( sf, simplify = FALSE )
	expect_equal( geo, as.character( geo2 ) )

