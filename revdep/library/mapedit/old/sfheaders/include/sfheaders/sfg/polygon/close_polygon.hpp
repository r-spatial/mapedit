#ifndef R_SFHEADERS_POLYGON_CLOSE_POLYGON_H
#define R_SFHEADERS_POLYGON_CLOSE_POLYGON_H

#include <Rcpp.h>
#include "geometries/utils/close/close.hpp"

namespace sfheaders {
namespace polygon_utils {

  template < int RTYPE >
  inline Rcpp::Matrix< RTYPE > close_polygon(
    Rcpp::Matrix< RTYPE >& mat,
    bool close = true
  ) {

    if( !close ) {
      return mat;
    }

    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;

    bool is_closed = true;

    Rcpp::Vector< RTYPE > first_row = mat( 0, Rcpp::_ );
    Rcpp::Vector< RTYPE > last_row = mat( n_row - 1, Rcpp::_ );

    for( i = 0; i < n_col; ++i ) {
      if( first_row[i] != last_row[i] ) {
        is_closed = false;
        break;
      }
    }

    if( !is_closed ) {
      Rcpp::Matrix< RTYPE > mat2( n_row + 1, n_col );
      for( i = 0; i < n_col; ++i ) {
        Rcpp::Vector< RTYPE > v( n_row + 1);
        Rcpp::Range rng( 0, n_row - 1 );

        v[ rng ] = mat( Rcpp::_, i );
        v[ n_row ] = first_row[i];
        mat2( Rcpp::_ , i ) = v;
      }
      geometries::utils::check_closed_rows( mat2.nrow() );
      return mat2;
    }

    // it is closed
    geometries::utils::check_closed_rows( mat.nrow() );  // #nocov
    return mat;  // #nocov
  }

  inline Rcpp::List close_polygon(
      Rcpp::List& lst,
      bool close = true
  ) {

    if( !close ) {
      return lst;
    }

    R_xlen_t n_items = lst.size();
    R_xlen_t i;

    for( i = 0; i < n_items; ++i ) {
      SEXP x = lst[i];
      switch( TYPEOF(x) ) {
      case INTSXP: {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );  // #nocov
        lst[i] = close_polygon( im, close ); // #nocov
        break; // #nocov
      }
      case REALSXP: {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
        lst[i] = close_polygon( nm, close );
        break;
      }
      case VECSXP: {
        Rcpp::List lst2 = Rcpp::as< Rcpp::List >( x );
        lst[i] = close_polygon( lst2 );
        break;
      }
      default: {
        Rcpp::stop("sfheaders - closing polygons requires matrices");  // #nocov
      }
      }
    }

    return lst;

  }

} // polygon
} // sfheaders

#endif
