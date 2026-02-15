#ifndef R_SFHEADERS_SFG_MULTIPOLYGONS_H
#define R_SFHEADERS_SFG_MULTIPOLYGONS_H

#include <Rcpp.h>
#include "sfheaders/sfg/multipolygon/sfg_multipolygon.hpp"

namespace sfheaders {
namespace sfg {

  inline Rcpp::List sfg_multipolygons( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs(n);

    SEXP geometry_cols = R_NilValue;
    SEXP polygon_id = R_NilValue;
    SEXP linestring_id = R_NilValue;

    for( i = 0; i < n; ++i ) {
      SEXP x = lst[i];
      sfcs[i] = sfheaders::sfg::sfg_multipolygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close );
    }
    return sfcs;
  }

  // inline Rcpp::List sfg_multipolygons( SEXP& obj, std::string xyzm, bool close = true ) {
  //   Rcpp::List lst = Rcpp::as< Rcpp::List >( obj );
  //   return sfg_multipolygons( lst, xyzm, close );
  // }


} // sfg
} // sfheaders

#endif
