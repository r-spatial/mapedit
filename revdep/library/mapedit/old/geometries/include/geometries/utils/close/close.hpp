#ifndef R_GEOMETRIES_UTILS_CLOSE_H
#define R_GEOMETRIES_UTILS_CLOSE_H

#include <Rcpp.h>

namespace geometries {
namespace utils {

  inline void check_closed_rows( R_xlen_t n_row ) {
    if( n_row < 4 ) {
      Rcpp::stop("geometries - closed shapes must have at least 4 rows");
    }
  }

  template< int RTYPE >
  inline bool matrix_is_closed(
    Rcpp::Matrix< RTYPE >& mat
  ) {
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
    return is_closed;
  }

  template < int RTYPE >
  inline Rcpp::Matrix< RTYPE > close_matrix(
    Rcpp::Matrix< RTYPE >& mat,
    bool& is_closed
  ) {
    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;

    if( !is_closed ) {
      Rcpp::Vector< RTYPE > first_row = mat( 0, Rcpp::_ );
      Rcpp::Matrix< RTYPE > mat2( n_row + 1, n_col );
      for( i = 0; i < n_col; ++i ) {
        Rcpp::Vector< RTYPE > nv( n_row + 1);
        Rcpp::Range rng( 0, n_row - 1 );

        nv[ rng ] = mat( Rcpp::_, i );
        nv[ n_row ] = first_row[i];
        mat2( Rcpp::_ , i ) = nv;
      }
      check_closed_rows( mat2.nrow() );
      return mat2;
    }
    // it is closed
    check_closed_rows( mat.nrow() );
    return mat;
  }

  template < int RTYPE >
  inline Rcpp::Matrix< RTYPE > close_matrix(
      Rcpp::Matrix< RTYPE >& mat
  ) {
    bool is_closed = matrix_is_closed( mat );
    return close_matrix( mat, is_closed );
  }

  inline Rcpp::List close_matrix(
      Rcpp::List& lst
  ) {

    R_xlen_t n_items = lst.size();
    R_xlen_t i;

    for( i = 0; i < n_items; ++i ) {
      SEXP x = lst[i];
      switch( TYPEOF(x) ) {
        case INTSXP: {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          lst[i] = close_matrix( im );
          break;
        }
        case REALSXP: {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          lst[i] = close_matrix( nm );
          break;
        }
        case VECSXP: {
          Rcpp::List lst2 = Rcpp::as< Rcpp::List >( x );
          lst[i] = close_matrix( lst2 );
          break;
        }
        default: {
          Rcpp::stop("geometries - closing shapes requires matrices"); // #nocov
        }
      }
    }
    return lst;
  }

} // utils
} // geometries

#endif
