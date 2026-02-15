## "sf attributes correctly assigned", {

	g <- '{"type": "Point", "coordinates": [100.0, 0.0]}'
	f <- '{"type": "Feature","properties": null,
		"geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}}'
	gc <- '{
	    "type": "GeometryCollection",
	    "geometries": [
	        {"type": "Point", "coordinates": [100.0, 0.0]},
	        {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
	        {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]}'
	fc <- '{
	  "type": "FeatureCollection",
	  "features": [
	  {
	    "type": "Feature",
	    "properties": {"foo" : "feature 1.1", "bar" : "feature 1.2"},
	    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
	  },{
	    "type": "Feature",
	    "properties": null,
	    "geometry": {"type": "LineString", "coordinates": [[103.0, 0.0], [102.0, 1.0]]}
	  },{
	    "type": "Feature",
		    "properties": {"foo" : "feature 3.1", "bar" : "feature 3.2"},
		    "geometry": {"type": "LineString", "coordinates": [[109.0, 0.0], [102.0, 1.0]]}
		}]}'
	fc1 <- '{
	  "type": "FeatureCollection",
	  "features": [{"type": "Feature","properties": null,
	    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}}]}'
	fgc <- '{
    "type" : "Feature",
	  "properties" : {},
	  "geometry" : {
	    "type": "GeometryCollection", "geometries": [
	      {"type": "Point", "coordinates": [100.0, 0.0]},
	      {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
	      {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]}}'

	fgc_mix <- '{
	  "type": "FeatureCollection",
	  "features": [
	  {
	    "type": "Feature",
	    "properties": {"foo" : "feature 1.1", "bar" : "feature 1.2"},
	    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
	  },{
	    "type": "Feature",
	    "properties": null,
	    "geometry": {"type": "LineString", "coordinates": [[103.0, 0.0], [102.0, 1.0]]}
	  },{
	    "type": "Feature",
		    "properties": {"foo" : "feature 3.1", "bar" : "feature 3.2"},
		    "geometry": {"type": "LineString", "coordinates": [[109.0, 0.0], [102.0, 1.0]]}
		},{
    "type" : "Feature",
	  "properties" : {},
	  "geometry" : {
	    "type": "GeometryCollection", "geometries": [
	      {"type": "Point", "coordinates": [100.0, 0.0]},
        {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
        {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}]}}
      ]}'

	gc_ml <- '{"type":"GeometryCollection","geometries":[{"type":"MultiLineString","coordinates":[[[0.0,0.0],[0.0,1.0],[1.0,1.0],[1.0,0.0],[0.0,0.0]]]}]}'

	sf_f <- geojson_sf(f)
	sf_g <- geojson_sf(g)
	sf_gc <- geojson_sf(gc)
	sf_fc1 <- geojson_sf(fc1)
	sf_fc <- geojson_sf(fc)
	sf_fgc <- geojson_sf(fgc)
	sf_fgc_mix <- geojson_sf(fgc_mix)
	sf_gc_ml <- geojson_sf(gc_ml)

	expect_true("sf" %in% class(sf_f))
	expect_true("sf" %in% class(sf_g))
	expect_true("sf" %in% class(sf_gc))
	expect_true("sf" %in% class(sf_fc1))
	expect_true("sf" %in% class(sf_fc))
	expect_true("sf" %in% class(sf_fgc))
	expect_true("sf" %in% class(sf_fgc_mix))
	expect_true("sf" %in% class(sf_gc_ml))

	expect_true("sfc_LINESTRING" %in% class(sf_f$geometry))
	expect_true("sfc_POINT" %in% class(sf_g$geometry))
	expect_true("sfc_GEOMETRY" %in% class(sf_gc$geometry))  ## TODO( should be GEOMETRYCOLLECTION) ?
	expect_true("sfc_POINT" %in% class(sf_fc1$geometry))
	expect_true("sfc_GEOMETRY" %in% class(sf_fc$geometry))  ## TODO( should be GEOMETRYCOLLECTION) ?
	expect_true("sfc_GEOMETRY" %in% class(sf_fgc$geometry))
	expect_true("sfc_GEOMETRY" %in% class(sf_fgc_mix$geometry))
	expect_true("sfc_GEOMETRY" %in% class(sf_gc_ml$geometry))   ## TODO( should be GEOMETRYCOLLECTION) ?

# 	expect_true(all.equal(attr(sf_f$geometry, "class"), c("sfc_LINESTRING", "sfc")))
# 	expect_true(all.equal(attr(sf_g$geometry, "class"), c("sfc_POINT", "sfc")))
#   expect_true(all.equal(attr(sf_gc$geometry, "class"), c("sfc_GEOMETRY", "sfc")))
# 	expect_true(all.equal(attr(sf_fc1$geometry, "class"), c("sfc_POINT", "sfc")))
# 	expect_true(all.equal(attr(sf_fc$geometry, "class"), c("sfc_GEOMETRY", "sfc")))
# 	expect_true(all.equal(attr(sf_fgc$geometry, "class"), c("sfc_GEOMETRY", "sfc")))

