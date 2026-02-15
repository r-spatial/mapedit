#ifndef R_GEOMETRIES_UTILS_UNIQUE_SORT_H
#define R_GEOMETRIES_UTILS_UNIQUE_SORT_H

#include <Rcpp.h>
#include "geometries/utils/attributes/attributes.hpp"

namespace geometries {
namespace utils {

   // issue 27 (sfheaders)
  template < typename T, int RTYPE >
  inline SEXP sexp_unique( Rcpp::Vector< RTYPE > x ) {
    std::set< T > seen;
    auto newEnd = std::remove_if( x.begin(), x.end(), [&seen]( const T value ) {
      if ( seen.find( value ) != std::end( seen ) ) {
        return true;
      }
      seen.insert( value );
      return false;
    });
    x.erase( newEnd, x.end() );
    return x;
  }


  // returns unique values in their original input order
  inline SEXP get_sexp_unique( SEXP s ) {

    SEXP s2 = Rcpp::clone( s );

    switch( TYPEOF( s2 ) ) {
    case LGLSXP: {
      return sexp_unique< bool, LGLSXP >( s2 );
    }
    case REALSXP: {
      return sexp_unique< double, REALSXP >( s2 );
    }
    case INTSXP: {
      // issue 89 (sfheaders - https://github.com/dcooley/sfheaders/issues/89)
      if( Rf_isFactor(s2) ) {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( s2 );
        Rcpp::List attributes = Rcpp::List::create(
          Rcpp::_["levels"] = iv.attr("levels"),
          Rcpp::_["class"] = iv.attr("class")
        );

        SEXP res = sexp_unique< int, INTSXP >( s2 );
        geometries::utils::attach_attributes( res, attributes  );
        return res;
      }


      return sexp_unique< int, INTSXP >( s2 );
    }
    case STRSXP: {
      return sexp_unique< char* , STRSXP >( s2 );
    }
    default: Rcpp::stop("geometries - unknown vector type");
    }
    return 0;
  }

} // utils
} // geometries


#endif
