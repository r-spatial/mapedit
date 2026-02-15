#ifndef R_SFHEADERS_SFG_POINTS_H
#define R_SFHEADERS_SFG_POINTS_H

#include <Rcpp.h>
#include "sfheaders/sfg/point/sfg_point.hpp"

namespace sfheaders {
namespace sfg {

  inline Rcpp::List sfg_points( Rcpp::List& lst, std::string xyzm ) {
    // assumes a lsit of POINT objects
    R_xlen_t n = lst.size();
    R_xlen_t i;
    Rcpp::List sfcs( n );

    SEXP geometry_cols = R_NilValue;
    for( i = 0; i < n; ++i ) {
      SEXP x = lst[ i ];
      sfcs[ i ] = sfheaders::sfg::sfg_point( x, geometry_cols, xyzm );
    }
    return sfcs;
  }

  template < int RTYPE >
  inline Rcpp::List sfg_points( Rcpp::Matrix< RTYPE >& mat, std::string xyzm ) {
    R_xlen_t n = mat.nrow();
    R_xlen_t i;
    Rcpp::List sfcs( n );

    for( i = 0; i < n; ++i ) {
      Rcpp::Vector< RTYPE > vec = mat( i, Rcpp::_ );
      sfcs[ i ] = sfheaders::sfg::sfg_point< RTYPE >( vec, xyzm );
    }
    return sfcs;
  }

  // inline Rcpp::List sfg_points( SEXP& obj, std::string xyzm ) {
  //   Rcpp::NumericMatrix mat = geometries::matrix::to_geometry_matrix( obj );
  //   return sfg_points( mat, xyzm );
  // }

} // sfg
} // sfheaders


#endif
