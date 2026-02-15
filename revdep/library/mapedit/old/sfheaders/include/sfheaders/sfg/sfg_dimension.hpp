#ifndef R_SFHEADERS_SFG_DIMENSION_H
#define R_SFHEADERS_SFG_DIMENSION_H

#include <Rcpp.h>

namespace sfheaders {
namespace sfg {

  inline void dimension_check( R_xlen_t& n ) {
    if( n < 2 || n > 4 ) {
      Rcpp::stop("sfheaders - invalid dimension ");
    }
  }

  inline std::string guess_xyzm( R_xlen_t n_col ) {

    switch( n_col ) {
    case 2: { return "XY"; }
    case 3: { return "XYZ"; }
    case 4: { return "XYZM"; }
    default: { Rcpp::stop("sfheaders - can't work out the dimension"); }
    }
    return ""; // #nocov - never reaches
  }

  // re-write
  // it can be XYM, and we'll know from R based on the x, y, m columns specified.
  // but if none are specified, and just 3 columns, it will defualt to XYZ
  //
  // for c++ will need an overloaded function, or a flag, to say 'm-first',
  // and R will use something similar after the x, y, m args are defined
  // then enter teh appropriate C++ function.


  inline std::string sfg_dimension(
      R_xlen_t& n,
      std::string xyzm = ""
  ) {

    if( !xyzm.empty() ) {
      return xyzm;
    }

    dimension_check( n );
    std::string dim = "XY";

    switch ( n ) {
    case 3: {
      return "XYZ";  // default to XYZ if dimension is not provided, and n == 3
    }
    case 4: {
      return "XYZM";
    }
    }
    return dim;
  }

  template< int RTYPE >
  inline std::string sfg_dimension( Rcpp::Vector< RTYPE >& vec, std::string xyzm = "" ) {
    R_xlen_t n = vec.size();
    return sfg_dimension( n, xyzm );
  }

  template< int RTYPE >
  inline std::string sfg_dimension( Rcpp::Matrix< RTYPE >& mat, std::string xyzm = "" ) {
    R_xlen_t n_col = mat.ncol();
    return sfg_dimension( n_col, xyzm );
  }

  inline std::string sfg_dimension( Rcpp::DataFrame& df, std::string xyzm = "" ) {
    R_xlen_t n_col = df.ncol();
    return sfg_dimension( n_col, xyzm );
  }

  inline std::string sfg_dimension( SEXP x, std::string xyzm = "" ) {

    switch ( TYPEOF( x ) ) {
    case INTSXP: {
    if( Rf_isMatrix( x ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
      return sfg_dimension( im, xyzm );
    } else {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
      return sfg_dimension( iv, xyzm );
    }
    }
    case REALSXP: {
    if( Rf_isMatrix( x ) ) {
      Rcpp::NumericMatrix im = Rcpp::as< Rcpp::NumericMatrix >( x );
      return sfg_dimension( im, xyzm );
    } else {
      Rcpp::NumericVector iv = Rcpp::as< Rcpp::NumericVector >( x );
      return sfg_dimension( iv, xyzm );
    }
    }
    case VECSXP: { // data.frame && list?
    if( Rf_inherits( x, "data.frame" ) ) {
      Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
      return sfg_dimension( df, xyzm );
    } else if ( Rf_isNewList( x ) ) {
      // we can just get the first element, because by this point
      // all the elements should have been made correctly (?)
      Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
      SEXP list_element = lst[ 0 ];
      return sfg_dimension( list_element, xyzm );
    } // else default


    }
    default: {
      Rcpp::stop("sfheaders - unsupported sfg type"); // #nocov
    }
    }

    return ""; // never reaches
  }

} // sfg
} // sfheaders

#endif
