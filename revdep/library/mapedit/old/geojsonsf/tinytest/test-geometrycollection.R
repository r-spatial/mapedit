## "expand_geometriesing preserves properties, ", {

	js <- '{
	"type": "GeometryCollection",
	"geometries": [
	{"type": "Point","coordinates": [100.0, 0.0]},
	{"type": "LineString","coordinates": [[101.0, 0.0],[102.0, 1.0]]},
	{"type" : "MultiPoint","coordinates" : [[0,0],[1,1],[2,2]]}]}'

	expect_true(nrow(geojson_sf(js, expand_geometries = F)) == 1)
	expect_true(nrow(geojson_sf(js, expand_geometries = T)) == 3)

	js <- '{"type":"Feature","properties":{"id":1},"geometry":{
	"type": "GeometryCollection",
	"geometries": [
  {"type": "Point","coordinates": [100.0, 0.0]},
	{"type": "LineString","coordinates": [[101.0, 0.0],[102.0, 1.0]]},
	{"type" : "MultiPoint","coordinates" : [[0,0],[1,1],[2,2]]}]}}'

	expect_true(nrow(geojson_sf(js, expand_geometries = F)) == 1)
	expect_true(nrow(geojson_sf(js, expand_geometries = T)) == 3)
	expect_true(unique(geojson_sf(js, expand_geometries = T)[['id']]) == 1)

	js <- '[{
	"type": "Feature",
	"properties" : {},
	"geometry": {"type": "Polygon","coordinates": [[[-10.0, -10.0],[10.0, -10.0],[10.0, 10.0],[-10.0, -10.0]]]}},{
	"type": "Feature",
	"properties" : {"id":1},
	"geometry": {"type": "MultiPolygon",
	"coordinates": [[[[180.0, 40.0], [180.0, 50.0], [170.0, 50.0],[170.0, 40.0], [180.0, 40.0]]],
	[[[-170.0, 40.0], [-170.0, 50.0], [-180.0, 50.0],[-180.0, 40.0], [-170.0, 40.0]]]]}},{
	"type": "FeatureCollection",
	"features": [{
	"type": "Feature",
	"properties": {"id" : 2, "value" : "foo"},
	"geometry": {"type": "Point","coordinates": [100.0, 0.0]}},{
	"type": "Feature",
	"properties": null,
	"geometry": {
	"type": "LineString",
	"coordinates": [[101.0, 0.0],[102.0, 1.0]]}}]},{
	"type": "GeometryCollection",
	"geometries": [
	  {"type": "Point","coordinates": [100.0, 0.0]},
	  {"type": "LineString","coordinates": [[101.0, 0.0],[102.0, 1.0]]},
	  {"type" : "MultiPoint","coordinates" : [[0,0],[1,1],[2,2]]}]},
	{"type": "Polygon","coordinates": [[[-10.0, -10.0],[10.0, -10.0],[10.0, 10.0],[-10.0, -10.0]]]}]'

	expect_true(nrow(geojson_sf(js, expand_geometries = F)) == 6)
	expect_true(nrow(geojson_sf(js, expand_geometries = T)) == 8)

