#ifndef R_GEOMETRIES_NULL_H
#define R_GEOMETRIES_NULL_H

#include "geometries/utils/sexp/sexp.hpp"

namespace geometries {
namespace utils {

  inline bool is_null_geometry( Rcpp::IntegerVector& iv ) {
    R_xlen_t n = iv.length();

    if ( iv[0] == NA_INTEGER || iv[1] == NA_INTEGER ) {
      return true;
    }

    if ( n == 0 ) {
      return true;
    }

    return false;
  }

  inline bool is_null_geometry( Rcpp::NumericVector& nv ) {
    R_xlen_t n = nv.length();

    if (ISNAN( nv[0] ) || ISNAN( nv[1] ) ) {
      return true;
    }

    if ( n == 0 ) {
      return true;
    }
    return false;
  }

} // utils
} // geometries

#endif
