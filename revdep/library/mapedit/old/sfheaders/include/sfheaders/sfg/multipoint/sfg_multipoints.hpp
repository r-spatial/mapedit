#ifndef R_SFHEADERS_SFG_MULTIPOINTS_H
#define R_SFHEADERS_SFG_MULTIPOINTS_H

#include <Rcpp.h>
#include "sfheaders/sfg/multipoint/sfg_multipoint.hpp"

namespace sfheaders {
namespace sfg {

  inline Rcpp::List sfg_multipoints( Rcpp::List& lst, std::string xyzm ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs(n);

    for( i = 0; i < n; ++i ) {
      Rcpp::NumericMatrix x = lst[i];    // multipoints are matrices
      sfcs[i] = sfheaders::sfg::sfg_multipoint( x, xyzm );
    }
    return sfcs;
  }

  // inline Rcpp::List sfg_multipoints( SEXP& obj, std::string xyzm ) {
  //   Rcpp::List lst = geometries::utils::as_list( obj );
  //   return sfg_multipoints( lst, xyzm );
  // }

} // sfg
} // sfheaders


#endif
