## v2.0.5

* updated `geojsonsf` dependency to fix missing Z/M values [issue 100](https://github.com/SymbolixAU/geojsonsf/issues/100)

## v2.0.4

* removed minimum C++ requirement
* `null` as the first element gets coerced to correct type [issue 79](https://github.com/SymbolixAU/geojsonsf/issues/79) (again)

## v2.0.3

* Validated LineString array [issue95](https://github.com/SymbolixAU/geojsonsf/issues/95)

## v2.0.2

* Fixed UTF-8 encoded strings on Windows [issue 90](https://github.com/SymbolixAU/geojsonsf/issues/90)

## v2.0.1

* links to geometries library
* reduced R dependency

## v2.0.0

* updated licence to MIT
* corrected incorrect CRS object to align with sf >= v0.9 [issue 83](https://github.com/SymbolixAU/geojsonsf/issues/83)
* moved to `tinytest` for test coverage
* removed `curl` dependency [issue 81](https://github.com/SymbolixAU/geojsonsf/issues/81)
* NULLs coerced to correct type [issue 79](https://github.com/SymbolixAU/geojsonsf/issues/79)

## v1.3.3

* removed Boost links
* updated tests ready for R v4.0.0

## v1.3.2

* using `rapidjson::FileReadStream` to read large files from disk [issue 64](https://github.com/SymbolixAU/geojsonsf/issues/64)
* `crs` and `proj4string` arguments added to `geojson_sf` and `geojson_sfc` 
* dependency on `sfheaders`
* fix crash due to factor levels [issue 62](https://github.com/SymbolixAU/geojsonsf/issues/62)
* fix invalid GeoJSON when NA elements [issue 63](https://github.com/SymbolixAU/geojsonsf/issues/63)


## v1.3

* restructured C++ src code for easier navigation and linking
* added dependency on jsonify v0.2 and fixed tests
* Factors now default to strings

## v1.2.2

* `digits` argument for rounding coordinates
* fix `'{"type":"FeatureCollection","features":[]}'` example [Issue 58](https://github.com/SymbolixAU/geojsonsf/issues/58)

## v1.2.1

* fixed crash when mis-specified MultiPolygon [issue 51](https://github.com/SymbolixAU/geojsonsf/issues/51)
* `df_geojson()` converts data.frame to GeoJSON POINTs

## v1.2.0

* `sf_geojson()` and `sfc_geojson()` output "json" class objects
* better handling of `null` objects / geometries [issue 36](https://github.com/SymbolixAU/geojsonsf/issues/36)
* `Date` and `POSIX` objects handled [issue 32](https://github.com/SymbolixAU/geojsonsf/issues/32)
* Z (elevation) and M attributes handled [issue 28](https://github.com/SymbolixAU/geojsonsf/issues/28)
* `simplify` argument for `geojson_sf()` to keep `FeatureCollection`s when converting `sf` without properties
* `sf_geojson()` performance improvments using `jsonify`

### Changes
* factors are now treated as-is (numeric)


## v1.1.0

* fix for GeometryCollection not being [closed correctly](https://github.com/SymbolixAU/geojsonsf/issues/26)

## v1.0.0

* `sf_geojson()` and `sfc_geojson()` to convert from `sf` to GeoJSON
* `geojson_sf()` and `geojson_sfc()` can now read from a url or file


## v0.2.0 First release

* `geojson_sf()`  convert GeoJSON to `sf` object
* `geojson_sfc()` convert GeoJSON to `sfc` object
* `geojson_wkt()` convert GeoJSON to `data.frame` with a Well-known text column


