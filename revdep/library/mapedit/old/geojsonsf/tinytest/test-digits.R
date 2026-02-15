##"digits are rounded", {

	## SF
	js <- '{"type":"Point","coordinates":[0.123456789,9.87654321]}'
	sf <- geojsonsf::geojson_sf( js )
	res <- geojsonsf::sf_geojson( sf, digits = 0 )
	expected <- '{"type":"Point","coordinates":[0.0,10.0]}'
	expect_equal( as.character( res ), expected )

	js <- '{"type":"Point","coordinates":[0.123456789,9.87654321]}'
	sf <- geojsonsf::geojson_sf( js )
	res <- geojsonsf::sf_geojson( sf, digits = 1 )
	expected <- '{"type":"Point","coordinates":[0.1,9.9]}'
	expect_equal( as.character( res ), expected )

  js <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":0},"geometry":{"type":"Point","coordinates":[0.123456789,9.87654321]}}]}'
  sf <- geojsonsf::geojson_sf( js )
  res <- geojsonsf::sf_geojson( sf, digits = 2 )
  expected <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":0.0},"geometry":{"type":"Point","coordinates":[0.12,9.88]}}]}'
  expect_equal( as.character( res), expected )

  ## SFC
  js <- '{"type":"Point","coordinates":[0.123456789,9.87654321]}'
  sfc <- geojsonsf::geojson_sfc( js )
  res <- geojsonsf::sfc_geojson( sfc, digits = 0 )
  expected <- '{"type":"Point","coordinates":[0.0,10.0]}'
  expect_equal( as.character( res ), expected )

  js <- '{"type":"Point","coordinates":[0.123456789,9.87654321]}'
  sfc <- geojsonsf::geojson_sfc( js )
  res <- geojsonsf::sfc_geojson( sfc, digits = 1 )
  expected <- '{"type":"Point","coordinates":[0.1,9.9]}'
  expect_equal( as.character( res ), expected )

  js <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":0},"geometry":{"type":"Point","coordinates":[0.123456789,9.87654321]}}]}'
  sfc <- geojsonsf::geojson_sfc( js )
  res <- geojsonsf::sfc_geojson( sfc, digits = 2 )
  expected <- '{"type":"Point","coordinates":[0.12,9.88]}'
  expect_equal( as.character( res), expected )

  ## DF
  df <- data.frame(lon = 1.23456789, lat = 9.87654321)
  res <- geojsonsf::df_geojson( df, lon = "lon", lat = "lat", digits = 0)
  expected <- '{"type":"Point","coordinates":[1.0,10.0]}'
  expect_equal( as.character( res ), expected )



