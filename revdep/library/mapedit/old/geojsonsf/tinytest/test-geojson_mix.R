##"mixture of GeoJSON objects in R", {

	geo_vec <- c("{\"type\":\"Point\",\"coordinates\":[-118.68152563269095,36.43764870908927]}",
					 "{\"type\":\"Point\",\"coordinates\":[-118.67408758213843,36.43366018922779]}",
					 "[{\"type\":\"Point\",\"coordinates\":[-118.67708346361097,36.44208638659282]},
					 {\"type\":\"Point\",\"coordinates\":[-118.67886661944996,36.44110273135671]},
					 {\"type\":\"Point\",\"coordinates\":[-118.68089232041565,36.44173155205561]}]")

	sf <- geojson_sf(geo_vec)
	sfc <- geojson_sfc(geo_vec)

	expect_true(nrow(sf) == length(sfc))
	expect_true(all.equal(class(sf), c("sf", "data.frame")))
	expect_true(all.equal(class(sfc), c("sfc_POINT", "sfc")))
	expect_true(round(sf[[1]][[1]][1], 2) == -118.68)
	expect_true(round(sf[[1]][[4]][2], 2) == 36.44)
	expect_true(all(sapply(sf$geometry, class)[2, ] == c("POINT", "POINT", "POINT", "POINT", "POINT")))

	geo_arr <- '[{"type":"Point","coordinates":[100.0,0.0]},{"type":"Point","coordinates":[200.0,0.0]}]'

	sf <- geojson_sf(geo_arr)
	sfc <- geojson_sfc(geo_arr)
  expect_true(nrow(sf) == 2)
  expect_true(nrow(sf) == length(sfc))
	expect_true(sf[[1]][[1]][1] == 100)
	expect_true(sf[[1]][[2]][1] == 200)
	expect_true(all(sapply(sf$geometry, class)[2, ] == c("POINT", "POINT")))

	gc_arr <- '[{"type": "GeometryCollection","geometries":[
    {"type":"Point","coordinates":[100.0,0.0]},
	  {"type":"LineString","coordinates":[[101.0, 0.0],[102.0,1.0]]},
	  {"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]},
    {"type":"GeometryCollection","geometries": [
	    {"type":"Point", "coordinates": [100.0, 0.0]},
	    {"type":"LineString","coordinates":[[201.0,0.0],[202.0,1.0]]},
	    {"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}]}]'

	sf <- geojson_sf(gc_arr)
	sfc <- geojson_sfc(gc_arr)

	expect_true(nrow(sf) == 2)
	expect_true(nrow(sf) == length(sfc))
	expect_true(attributes(sf[[1]])[['class']][1] == "sfc_GEOMETRY")
	expect_true(attributes(sf[[1]][[1]])[['class']][2] == "GEOMETRYCOLLECTION")
	expect_true(all(sapply(sf$geometry, class)[2, ] == c("GEOMETRYCOLLECTION", "GEOMETRYCOLLECTION")))

	geo_mix <-  '[{"type": "GeometryCollection","geometries": [
      {"type": "Point", "coordinates": [100.0, 0.0]},
	    {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
	    {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]
	  },{"type": "GeometryCollection","geometries": [
	    {"type": "Point", "coordinates": [100.0, 0.0]},
	    {"type": "LineString", "coordinates": [[201.0, 0.0], [202.0, 1.0]]},
	    {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]},
	  {"type": "Point","coordinates": [100.0, 0.0]},
	  {"type": "Point","coordinates": [ 200.0, 0.0]}]'

	geo_mix <- c(geo_mix, geo_vec)
	sf <- geojson_sf(geo_mix)
	sfc <- geojson_sfc(geo_mix)
	expect_true(nrow(sf) == 9)
	expect_true(nrow(sf) == length(sfc))
	expect_true(all(sapply(sf$geometry, class)[2, ] == c("GEOMETRYCOLLECTION", "GEOMETRYCOLLECTION", "POINT", "POINT","POINT", "POINT","POINT", "POINT","POINT")))

	fcarr <- '[{"type": "FeatureCollection","features":[
      {"type":"Feature","properties":null,"geometry":{"type":"Point","coordinates":[100.0,0.0]}},
	    {"type":"Feature","properties":null,"geometry":{"type":"LineString","coordinates":[[201.0,0.0],[102.0,1.0]]}},
	    {"type":"Feature","properties":null,"geometry":{"type":"LineString","coordinates":[[301.0,0.0],[102.0,1.0]]}}]},
	  {"type":"FeatureCollection","features": [
		  {"type":"Feature","properties":null,"geometry":{"type":"Point","coordinates":[100.0,0.0]}},
		  {"type":"Feature","properties":null,"geometry":{"type":"LineString","coordinates":[[501.0,0.0],[102.0,1.0]]}},
		  {"type":"Feature","properties":null,"geometry":{"type":"LineString","coordinates":[[601.0,0.0],[102.0,1.0]]}}]}]'
	sf <- geojson_sf(fcarr)
	expect_true(nrow(sf) == 6)
	expect_true(all(sapply(sf$geometry, class)[2, ] == c("POINT", "LINESTRING", "LINESTRING", "POINT", "LINESTRING", "LINESTRING")))

