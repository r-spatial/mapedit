#ifndef R_SFHEADERS_SFG_ATTRIBUTES_H
#define R_SFHEADERS_SFG_ATTRIBUTES_H

#include <Rcpp.h>

namespace sfheaders {
namespace sfg {

  template <int RTYPE>
  inline Rcpp::CharacterVector sfgClass( Rcpp::Vector<RTYPE> v ) {
    return v.attr("class");
  }

  inline Rcpp::CharacterVector getSfgClass( SEXP sf ) {

    switch( TYPEOF(sf) ) {
    case REALSXP:
      return sfgClass<REALSXP>( sf );
    case VECSXP:
      return sfgClass<VECSXP>( sf );
    case INTSXP:
      return sfgClass<INTSXP>( sf );
    default: Rcpp::stop("unknown sf type");
    }
    return "";
  }

  // inline Rcpp::CharacterVector sfg_attributes( std::string& dimension, std::string& geom_type ) {
  //   return Rcpp::CharacterVector::create( dimension, geom_type, "sfg" );
  // }
  //
  // template < typename T >
  // inline SEXP attach_sfg_attribute( T x, std::string& dim, std::string& geom_type ) {
  //   x.attr("class") = sfg_attributes( dim, geom_type );
  //   return x;
  // }
  //
  // inline SEXP attach_sfg_attribute( SEXP x, std::string& dim, std::string& geom_type ) {
  //   switch (TYPEOF( x ) ) {
  //   case INTSXP: {
  //   if( !Rf_isMatrix( x ) ) {
  //     return attach_sfg_attribute< Rcpp::IntegerVector >( x, dim, geom_type );
  //   } else {
  //     return attach_sfg_attribute< Rcpp::IntegerMatrix >( x, dim, geom_type );
  //   }
  //   }
  //   case REALSXP: {
  //   if( !Rf_isMatrix( x ) ) {
  //     return attach_sfg_attribute< Rcpp::NumericVector>( x, dim, geom_type );
  //   } else {
  //     return attach_sfg_attribute< Rcpp::NumericMatrix >( x, dim, geom_type );
  //   }
  //   }
  //   case VECSXP: {
  //     return attach_sfg_attribute< Rcpp::List >( x, dim, geom_type );
  //   }
  //   default: {
  //     Rcpp::stop("sfheaders - attach_attribute invalid type");
  //   }
  //   }
  //   return Rcpp::List::create(); // never reaches
  // }


} // sfg
} // sfheaders


#endif
