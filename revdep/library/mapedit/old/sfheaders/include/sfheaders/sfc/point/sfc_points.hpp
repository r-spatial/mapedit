#ifndef R_SFHEADERS_SFC_POINTS_H
#define R_SFHEADERS_SFC_POINTS_H

#include <Rcpp.h>
#include "sfheaders/sfc/point/sfc_point.hpp"

namespace sfheaders {
namespace sfc {

  inline Rcpp::List sfc_points( Rcpp::List& lst, std::string xyzm ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs( n );

    SEXP geometry_cols = R_NilValue;

    for( i = 0; i < n; ++i ) {
      SEXP x = lst[i];
      sfcs[i] = sfheaders::sfc::sfc_point( x, geometry_cols, xyzm );
    }
    return sfcs;
  }

} // sfc
} // sfheaders

#endif
