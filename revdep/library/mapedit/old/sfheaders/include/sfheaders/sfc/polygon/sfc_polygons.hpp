#ifndef R_SFHEADERS_SFC_POLYGONS_H
#define R_SFHEADERS_SFC_POLYGONS_H

#include <Rcpp.h>
#include "sfheaders/sfc/polygon/sfc_polygon.hpp"

namespace sfheaders {
namespace sfc {


  inline Rcpp::List sfc_polygons( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs(n);

    SEXP geometry_cols = R_NilValue;
    SEXP polygon_id = R_NilValue;
    SEXP linestring_id = R_NilValue;

    for( i = 0; i < n; ++i ) {
      SEXP x = lst[i];
      sfcs[i] = sfheaders::sfc::sfc_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close );
    }
    return sfcs;
  }

} // sfc
} // sfheaders

#endif
