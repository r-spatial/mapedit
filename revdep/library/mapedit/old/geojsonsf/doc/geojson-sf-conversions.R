## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(geojsonsf)
# library(sf)

## -----------------------------------------------------------------------------
p <- '{"type":"Point","coordinates":[0,0]}'
sf <- geojson_sf(p)
head(sf)

## -----------------------------------------------------------------------------
gc <- '{
  "type": "GeometryCollection",
  "geometries": [
    {"type": "Point", "coordinates": [100.0, 0.0]},
    {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
    {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}
  ]
}'
sf <- geojson_sf(gc)
head(sf)

## -----------------------------------------------------------------------------
gc <- '{
  "type": "GeometryCollection",
  "geometries": [
    {"type": "Point", "coordinates": [100.0, 0.0]},
    {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]},
    {"type" : "MultiPoint", "coordinates" : [[0,0], [1,1], [2,2]]}
  ]
}'
sf <- geojson_sf(gc, expand_geometries = T)
head(sf)

## -----------------------------------------------------------------------------
f <- '{
	"type": "Feature",
	"properties": {"id":1,"value":100,"text":"the quick brown fox"},
	"geometry": {
	  "type": "LineString", 
	  "coordinates": [[101.0, 0.0], [102.0, 1.0]]
	  }
	}'
sf <- geojson_sf(f)
head(sf)

## -----------------------------------------------------------------------------
fc <- '{
  "type": "FeatureCollection",
  "features": [
  {
    "type": "Feature",
    "properties": {"foo" : "feature 1.1", "bar" : "feature 1.2"},
    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
  },
  {
    "type": "Feature",
    "properties": null,
    "geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}
  },
  {
    "type": "Feature",
	    "properties": {"foo" : "feature 3.1", "bar" : "feature 3.2"},
	    "geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}
	}
 ]
}'
sf <- geojson_sf(fc)
head(sf)


## -----------------------------------------------------------------------------
geo <- c(p, gc, f, fc)
sf <- geojson_sf(geo)
head(sf)

## -----------------------------------------------------------------------------

js <- '[
{
  "type": "FeatureCollection",
  "features": [
  {
    "type": "Feature",
    "properties": null,
    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
  },
  {
    "type": "Feature",
    "properties": null,
    "geometry": {"type": "LineString", "coordinates": [[201.0, 0.0], [102.0, 1.0]]}
  },
  {
    "type": "Feature",
	    "properties": null,
	    "geometry": {"type": "LineString", "coordinates": [[301.0, 0.0], [102.0, 1.0]]}
	}
 ]
},
{
  "type": "FeatureCollection",
	"features": [
	{
	  "type": "Feature",
	  "properties": null,
	  "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
	},
	{
	  "type": "Feature",
	  "properties": null,
	  "geometry": {"type": "LineString", "coordinates": [[501.0, 0.0], [102.0, 1.0]]}
	},
	{
	  "type": "Feature",
	  "properties": null,
	  "geometry": {"type": "LineString", "coordinates": [[601.0, 0.0], [102.0, 1.0]]}
	}
  ]
}
]'

sf <- geojson_sf(js)
head(sf)

## -----------------------------------------------------------------------------
sf <- geojson_sf(system.file("examples", "geo_melbourne.geojson", package = "geojsonsf"))
head(sf)

## -----------------------------------------------------------------------------
geo <- sf_geojson(sf)
substr(geo, 1, 80)

## -----------------------------------------------------------------------------
geo <- sf_geojson(sf, atomise = T)

## -----------------------------------------------------------------------------
substr(geo[1], 1, 80)
substr(geo[2], 1, 80)

## -----------------------------------------------------------------------------
fc <- '{
  "type": "FeatureCollection",
  "features": [
  {
    "type": "Feature",
    "properties": {"foo" : "feature 1.1", "bar" : "feature 1.2"},
    "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
  },
  {
    "type": "Feature",
    "properties": null,
    "geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}
  },
  {
    "type": "Feature",
	    "properties": {"foo" : "feature 3.1", "bar" : "feature 3.2"},
	    "geometry": {"type": "LineString", "coordinates": [[101.0, 0.0], [102.0, 1.0]]}
	}
 ]
}'
df <- geojson_wkt(fc)
head(df)

