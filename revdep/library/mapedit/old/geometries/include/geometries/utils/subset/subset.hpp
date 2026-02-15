#ifndef R_GEOMETRIES_UTILS_SUBSETTING_H
#define R_GEOMETRIES_UTILS_SUBSETTING_H

#include <Rcpp.h>

namespace geometries {
namespace utils {

  // template < int RTYPE >
  // inline Rcpp::Vector< RTYPE > subset_vector(
  //     Rcpp::Vector< RTYPE >& v,
  //     Rcpp::Range& rng
  // ) {
  //   return v[ rng ];
  // }
  //
  // inline SEXP subset_vector(
  //   SEXP& x,
  //   int& start,
  //   int& end
  // ) {
  //   Rcpp::Range rng = Rcpp::Range( start, end );
  //
  //   switch( TYPEOF( x ) ) {
  //     case LGLSXP: {
  //       Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( x );
  //       return subset_vector( lv, rng );
  //     }
  //     case INTSXP: {
  //       Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
  //       return subset_vector( iv, rng );
  //     }
  //     case REALSXP: {
  //       Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
  //       return subset_vector( nv, rng );
  //     }
  //     case STRSXP: {
  //       Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( x );
  //       return subset_vector( sv, rng ); // shouldn't get here? should only be numerics
  //     }
  //     default: {
  //       Rcpp::stop("geometries - unsupported vector type for subsetting");
  //     }
  //   }
  //   return Rcpp::List::create(); // never reaches
  // }
  //
  // inline Rcpp::List subset_dataframe(
  //   Rcpp::DataFrame& df,
  //   Rcpp::StringVector& cols,
  //   int& start,
  //   int& end
  // ) {
  //
  //   R_xlen_t n_cols = cols.size();
  //   Rcpp::IntegerVector row_names = Rcpp::seq( start + 1, end + 1 );
  //   R_xlen_t i;
  //   Rcpp::List df_subset( n_cols );
  //   for( i = 0; i < n_cols; ++i ) {
  //     Rcpp::String this_col = cols[ i ];
  //
  //     SEXP this_vec = df[ this_col ];
  //     df_subset[ i ] = subset_vector( this_vec, start, end );
  //   }
  //   df_subset.names() = cols;
  //   return df_subset;
  // }
  //
  // inline Rcpp::List subset_dataframe(
  //     Rcpp::DataFrame& df,
  //     int& start,
  //     int& end
  // ) {
  //   Rcpp::StringVector cols = df.names();
  //   return subset_dataframe( df, cols, start, end );
  // }

} // utils
} // geometries


#endif
