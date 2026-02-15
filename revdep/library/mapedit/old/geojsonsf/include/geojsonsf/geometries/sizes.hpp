#ifndef GEOJSONSF_GEOMETRY_SIZES_H
#define GEOJSONSF_GEOMETRY_SIZES_H

#include <Rcpp.h>

namespace geojsonsf {
namespace sizes {

  // determines the size of the geometry for MULTI objects
  // MULTIPOINT - number of points (vector size)
  // MULTILINESTRING - number of lines ( matrix rows )
  // MULTIPOLYGON - number of polygons ( list size? )
  inline int geometry_size(SEXP& sfg, std::string& geom_type, Rcpp::CharacterVector& cls) {

  	if (geom_type == "MULTIPOINT") {
  		Rcpp::NumericMatrix mat = Rcpp::as< Rcpp::NumericMatrix >( sfg );
  		return mat.nrow();

  	} else if (geom_type == "MULTILINESTRING") {
  		Rcpp::List multiline = Rcpp::as< Rcpp::List >( sfg );
  		return multiline.size();

  	} else if (geom_type == "MULTIPOLYGON") {
  		Rcpp::List multipolygon = Rcpp::as< Rcpp::List >( sfg );
  		return multipolygon.size();
  	}
  	return 1;
  }

} // namespace sizes
} // namespace geojsonsf


#endif
