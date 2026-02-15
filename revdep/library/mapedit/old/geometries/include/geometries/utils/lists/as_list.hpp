#ifndef R_GEOMETRIES_UTILS_LISTS_AS_LIST_H
#define R_GEOMETRIES_UTILS_LISTS_AS_LIST_H

namespace geometries {
namespace utils {

  template < int RTYPE >
  inline Rcpp::List as_list( Rcpp::Vector< RTYPE >& x ) {

    R_xlen_t n_col = x.length();
    R_xlen_t i;
    Rcpp::List res( n_col );
    for( i = 0; i < n_col; ++i ) {
      res[ i ] = x[ i ];
    }
    return res;
  }

  template < int RTYPE >
  inline Rcpp::List as_list( Rcpp::Matrix< RTYPE >& x ) {

    R_xlen_t n_col = x.ncol();
    R_xlen_t i;
    Rcpp::List res( n_col );
    for( i = 0; i < n_col; ++i ) {
      Rcpp::Vector< RTYPE > v = x( Rcpp::_, i );
      res[ i ] = v;
    }

    //if there are names, attach the names
    if( !Rf_isNull( x.attr("dimnames") ) ) {
      Rcpp::List dims = x.attr("dimnames");
      Rcpp::StringVector n = dims[1];
      res.names() = n;
    }

    return res;
  }

  inline Rcpp::List as_list( SEXP& x, bool keep_names = true ) {

    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          return as_list( im );
        } else {
          Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
          return as_list( iv );
        }
      }
      case REALSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return as_list( nm );
        } else {
          Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
          return as_list( nv );
        }
      }
      case VECSXP: {
        return Rcpp::as< Rcpp::List >( x );
      }
      default: {
        Rcpp::stop("geometries - unknown object type for converting to list");
      }
    }
    return Rcpp::List::create(); // never reaches
  }

} // utils
} // geometries

#endif
