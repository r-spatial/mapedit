#ifndef R_SFHEADERS_SFG_LINESTRINGS_H
#define R_SFHEADERS_SFG_LINESTRINGS_H

#include <Rcpp.h>
#include "sfheaders/sfg/linestring/sfg_linestring.hpp"

namespace sfheaders {
namespace sfg {

  inline Rcpp::List sfg_linestrings( Rcpp::List& lst, std::string xyzm ) {
    R_xlen_t n = lst.size();

    R_xlen_t i;
    Rcpp::List sfcs(n);

    for( i = 0; i < n; ++i ) {
      Rcpp::NumericMatrix x = lst[i];    // linestrings are matrices
      sfcs[i] = sfheaders::sfg::sfg_linestring( x, xyzm );
    }
    return sfcs;
  }

  // inline Rcpp::List sfg_linestrings( SEXP& obj, std::string xyzm ) {
  //   Rcpp::List lst = Rcpp::as< Rcpp::List >( obj );
  //   return sfg_linestrings( lst, xyzm );
  // }

} // sfg
} // sfheaders


#endif
