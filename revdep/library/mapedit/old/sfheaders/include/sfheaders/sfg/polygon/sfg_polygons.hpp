#ifndef R_SFHEADERS_SFG_POLYGONS_H
#define R_SFHEADERS_SFG_POLYGONS_H

#include <Rcpp.h>
#include "sfheaders/sfg/polygon/sfg_polygon.hpp"

namespace sfheaders {
namespace sfg {

  inline Rcpp::List sfg_polygons( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs(n);

    SEXP geometry_cols = R_NilValue;
    SEXP linestring_id = R_NilValue;

    for( i = 0; i < n; ++i ) {
      SEXP x = lst[i];  // polygon is a list of matrices
      sfcs[i] = sfheaders::sfg::sfg_polygon( x, geometry_cols, linestring_id, xyzm, close );
    }
    return sfcs;
  }

  // inline Rcpp::List sfg_polygons( SEXP& obj, std::string xyzm, bool close = true ) {
  //   Rcpp::List lst = Rcpp::as< Rcpp::List >( obj );
  //   return sfg_polygons( lst, xyzm, close );
  // }

} // sfg
} // sfheaders

#endif
