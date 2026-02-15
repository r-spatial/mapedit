##"ints vs numerics read correctly", {

	## TODO Integers
	# geo <- '{"type":"Point","coordinates":[0,0]}'
	# expect_equal(geojson_sf( geo )[[1]], geojson_sfc( geo ) )
	# expect_true(is.integer( geojson_sfc( geo )[[1]] ) )

	pt <- '{"type":"Point","coordinates":[0.1,0.1]}'
	expect_equal(geojson_sf( pt )[[1]], geojson_sfc( pt ) )
	expect_false(is.integer( geojson_sfc( pt )[[1]] ) )

	## TODO Integers
	# geo <- '{"type":"MultiPoint","coordinates":[[0,0],[1,1]]}'
	# expect_equal( geojson_sf( geo )[[1]], geojson_sfc( geo ) )
	# expect_true( is.integer( geojson_sfc( geo )[[1]] ) )

	mpt <- '{"type":"MultiPoint","coordinates":[[0.1,0],[1,1]]}'
	expect_equal( geojson_sf( mpt )[[1]], geojson_sfc( mpt ) )
	expect_false( is.integer( geojson_sfc( mpt )[[1]] ) )

	## TODO Integers
	# geo <- '{"type":"LineString","coordinates":[[0,0],[1,1]]}'
	# expect_equal( geojson_sf( geo )[[1]], geojson_sfc( geo ) )
	# expect_true( is.integer( geojson_sfc( geo )[[1]] ) )

	ls <- '{"type":"LineString","coordinates":[[0.1,0],[1,1]]}'
	expect_equal( geojson_sf( ls )[[1]], geojson_sfc( ls ) )
	expect_false( is.integer( geojson_sfc( ls )[[1]] ) )

	geo <- '{"type":"LineString","coordinates":[[0,0.1],[1,1]]}'
	expect_equal( geojson_sf( geo )[[1]], geojson_sfc( geo ) )
	expect_false( is.integer( geojson_sfc( geo )[[1]] ) )

	## TODO integers
	# geo <- '{"type":"MultiLineString","coordinates":[[[0,0],[1,1]]]}'
	# expect_equal( geojson_sf( geo )[[1]], geojson_sfc( geo ) )
	# expect_true( is.integer( geojson_sfc( geo )[[1]] ) )

	mls <- '{"type":"MultiLineString","coordinates":[[[0.0,0],[1,1]]]}'
	expect_equal( geojson_sf( mls )[[1]], geojson_sfc( mls ) )
	expect_false( is.integer( geojson_sfc( mls )[[1]] ) )

	ply <- '{"type":"Polygon","coordinates":[[[0,0],[0,1],[1,1],[1,0],[0,0]]]}'
	expect_equal( geojson_sf( ply )[[1]], geojson_sfc( ply ) )
	expect_false( is.integer( geojson_sfc( ply )[[1]] ) )

	mply <- '{"type":"MultiPolygon","coordinates":[[[[0,0],[0,1],[1,1],[1,0],[0,0]],[[2,2],[2,3],[3,3],[3,2],[2,2]]]]}'
	expect_equal( geojson_sf( mply )[[1]], geojson_sfc( mply ) )
	expect_false( is.integer( geojson_sfc( mply )[[1]] ) )

## "the geoms I test in mapdeck work", {

	expect_silent( geojsonsf::geojson_sf('{"type":"Point","coordinates":[0,0]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"MultiPoint","coordinates":[[0,0],[1,1]]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"LineString","coordinates":[[0,0],[1,1]]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"MultiLineString","coordinates":[[[0,0],[1,1]],[[0,0]]]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"Polygon","coordinates":[[[0,0],[0,1],[1,1],[1,0],[0,0]]]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"MultiPolygon","coordinates":[[[[0,0],[0,1],[1,1],[1,0],[0,0]],[[2,2],[2,3],[3,3],[3,2],[2,2]]]]}') )
	expect_silent( geojsonsf::geojson_sf('{"type":"MultiPolygon","coordinates":[[[[0,0],[0,1],[1,1],[1,0],[0,0]],[[0,0],[0,-1],[-1,-1],[1,0],[0,0]]]]}') )

## issue 79
##"NULLs coerced to correct type",{

	gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":1.0,"b":true,"c":"hello"},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":null,"b":null,"c":null},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}
]}'

	sf <- geojson_sf(gj)
	expect_true( is.numeric( sf$a ) )
	expect_true( is.logical( sf$b ) )
	expect_true( is.character( sf$c ) )

	## First entry is null
	gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":null,"b":true,"c":"hello"},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":1.0,"b":null,"c":null},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}
]}'

	sf <- geojson_sf(gj)
	expect_true( is.numeric( sf$a ) )
	expect_true( is.logical( sf$b ) )
	expect_true( is.character( sf$c ) )

	## First few entries are null
	gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":null,"b":true,"c":"hello"},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":null,"b":null,"c":null},"geometry":{"type":"Point","coordinates":[1.0,1.0]}},
	{"type":"Feature","properties":{"a":1.0,"b":null,"c":null},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}
]}'

	sf <- geojson_sf(gj)
	expect_true( is.numeric( sf$a ) )
	expect_true( is.logical( sf$b ) )
	expect_true( is.character( sf$c ) )

	## Issue 90
	## UTF8-encoded strings
	geojson <- enc2utf8('{"type":"Feature","geometry":{"type":"Point","coordinates":[7.624747,51.954044]},"properties":{"örtchen":"Münster"}}')
	out <- geojson_sf( geojson )
	expect_equal(out[1][[1]], "Münster")
	expect_equal(names(out)[1], "örtchen")

## Issue 170
## Logicals correctly converted
gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":true},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":false},"geometry":{"type":"Point","coordinates":[1.0,1.0]}},
  {"type":"Feature","properties":{"a":false},"geometry":{"type":"Point","coordinates":[2.0,2.0]}},
  {"type":"Feature","properties":{"a":true},"geometry":{"type":"Point","coordinates":[3.0,3.0]}}
]}'

sf <- geojson_sf(gj)
expect_true(is.logical(sf$a))
expect_equal(sf$a, c(TRUE, FALSE, FALSE, TRUE))

## works with all false
gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":false},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":false},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}
]}'

sf <- geojson_sf(gj)
expect_true(is.logical(sf$a))

## works with all true
gj <- '{"type":"FeatureCollection","features":[
  {"type":"Feature","properties":{"a":true},"geometry":{"type":"Point","coordinates":[0.0,0.0]}},
  {"type":"Feature","properties":{"a":true},"geometry":{"type":"Point","coordinates":[1.0,1.0]}}
]}'

sf <- geojson_sf(gj)
expect_true(is.logical(sf$a))
