## "geojson property types correctly captured in df", {

	## int & strings
	geo <- '{"type":"Feature","properties":{"id":1,"val":"a"},"geometry":{"type":"Point","coordinates":[0,0]}}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(nrow(sf) == 1)
	expect_true(nrow(sf) == length(sfc))
	expect_true(all(c("geometry", "id", "val") %in% names(sf)))
	expect_true(is.numeric(sf[['id']]))

	## null property
	geo <- '{"type":"Feature","properties":null,"geometry":{"type":"Point","coordinates":[0,0]}}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(nrow(sf) == length(sfc))
	expect_true(all(names(sf) %in% c("geometry")))

	## null property value
	geo <- '{"type":"Feature","properties":{"id":null},"geometry":{"type":"Point","coordinates":[0,0]}}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(nrow(sf) == length(sfc))
	expect_true(all(names(sf) %in% c("geometry", "id")))

	## logical property value
	geo <- '{"type":"Feature","properties":{"id":true},"geometry":{"type":"Point","coordinates":[0,0]}}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(nrow(sf) == length(sfc))
	expect_true(all(names(sf) %in% c("geometry", "id")))
	expect_true(is.logical(sf[['id']]))

	geo <- '{"type":"Feature","properties":{"id":{}},"geometry":{"type":"Point","coordinates":[0,0]}}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(nrow(sf) == length(sfc))

	## Different types for teh same property
	geo <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}},
	{"type":"Feature","properties":{"id":"a"},"geometry":{"type":"Point","coordinates":[1,1]}}]}'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(is.character(sf$id))
	expect_true(nrow(sf) == length(sfc))

	geo <- '[{"type":"Feature","properties":{"id":1},"geometry":{"type":"Point","coordinates":[0,0]}},
  {"type" : "Feature","properties" : { "id" : true},"geometry" : { "type" : "Point", "coordinates" : [2,2] }}]'
	sf <- geojson_sf(geo)
	expect_true(is.character(sf$id))

	geo <- '[{"type" : "Feature","properties" : { "id" : null},"geometry" : { "type" : "Point", "coordinates" : [1,0] }},
	{"type" : "Feature","properties" : { "id" : null},"geometry" : { "type" : "Point", "coordinates" : [0,0] }}]'
	sf <- geojson_sf(geo)
	sfc <- geojson_sfc(geo)
	expect_true(sum(is.na(sf$id)) == 2)
	expect_true(nrow(sf) == length(sfc))

	geo <- '[{"type" : "Feature","properties" : { "id" : "a"},"geometry" : { "type" : "Point", "coordinates" : [0,1] }},
  {"type" : "Feature","properties" : { "id" : 1},"geometry" : { "type" : "Point", "coordinates" : [0,0] }}]'
	sf <- geojson_sf(geo)
	expect_true(is.character(sf$id))

	##"object and array properties are strings", {

	geo <- '{"type": "Feature","properties":{"id":[1,2,3],"name":{"foo":"bar"}},"geometry":{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]}}'
	sf <- geojson_sf(geo)
	expect_true(sf[1, ]$id == "[1,2,3]")
	expect_true(sf[1, ]$name == '{"foo":"bar"}')

	geo <- '[{"type" : "Feature","properties" : { "id" : 1 },"geometry" : { "type" : "Point", "coordinates" : [0,1] }},
	{"type" : "Feature","properties" : { "id" : {"a":"b"}},"geometry" : { "type" : "Point", "coordinates" : [0,0] }}]'
	sf <- geojson_sf(geo)
	expect_true(is.character(sf$id))

