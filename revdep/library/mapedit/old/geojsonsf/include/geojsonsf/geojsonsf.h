#ifndef GEOJSONSF_H
#define GEOJSONSF_H

#include <Rcpp.h>

// [[Rcpp::depends(rapidjsonr)]]
// [[Rcpp::depends(sfheaders)]]

namespace geojsonsf {
  const Rcpp::String INPUT = "4326";
  const Rcpp::String WKT = R"(GEOGCS["WGS 84",
      DATUM["WGS_1984",
        SPHEROID["WGS 84",6378137,298.257223563,
          AUTHORITY["EPSG","7030"]],
        AUTHORITY["EPSG","6326"]],
      PRIMEM["Greenwich",0,
        AUTHORITY["EPSG","8901"]],
      UNIT["degree",0.0174532925199433,
        AUTHORITY["EPSG","9122"]],
      AXIS["Latitude",NORTH],
      AXIS["Longitude",EAST],
    AUTHORITY["EPSG","4326"]])";

  const std::string PROJ4STRING = "+proj=longlat +datum=WGS84 +no_defs";


  inline void attach_class( Rcpp::StringVector& geojson ) {
  	geojson.attr("class") = Rcpp::CharacterVector::create("geojson","json");
  }
}

#define UNKNOWN            0
#define POINT              1
#define MULTIPOINT         2
#define LINESTRING         3
#define MULTILINESTRING    4
#define POLYGON            5
#define MULTIPOLYGON       6
#define GEOMETRYCOLLECTION 7

#endif
