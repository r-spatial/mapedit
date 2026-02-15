#ifndef R_GEOMETRIES_UTILS_H
#define R_GEOMETRIES_UTILS_H

#include <Rcpp.h>
#include "geometries/utils/columns/columns.hpp"
#include "geometries/utils/lines/lines.hpp"
#include "geometries/utils/lists/list.hpp"
#include "geometries/utils/matrix/matrix.hpp"
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/subset/subset.hpp"
#include "geometries/utils/unique/unique.hpp"
#include "geometries/utils/vectors/vectors.hpp"
#include "geometries/utils/dataframe/dataframe.hpp"

namespace geometries {
namespace utils {

  inline void column_check( SEXP x, SEXP cols ) {
    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    R_xlen_t n = geometries::utils::sexp_length( cols );
    if( n > n_col ) {
      Rcpp::stop("geometries - number of columns requested is greater than those available");
    }

    if( TYPEOF( cols ) == INTSXP ) {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( cols );
      int m = Rcpp::max( iv );
      if( m > ( n_col - 1 ) || m < 0 ) {
        Rcpp::stop("geometries - invalid geometry column index");
      }
    }
  }

  // checks if an integer column index exists
  inline void column_exists( SEXP& x, int& cols ) {
    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    if( cols > ( n_col - 1 ) ) {
      Rcpp::stop("geometries - column index doesn't exist");
    }
  }

  inline void column_exists( Rcpp::List& x, Rcpp::IntegerVector& cols ) {
    R_xlen_t n_col = x.length();
    R_xlen_t max_id = Rcpp::max( cols );
    if( max_id > ( n_col - 1 ) ) {
      Rcpp::stop("geometries - column index doesn't exist");
    }
  }

  inline void column_exists( SEXP& x, Rcpp::IntegerVector& cols ) {
    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    R_xlen_t max_id = Rcpp::max( cols );
    if( max_id > ( n_col - 1 ) ) {
      Rcpp::stop("geometries - column index doesn't exist");
    }
  }

} // namespace utils
} // namespace geometries

#endif
