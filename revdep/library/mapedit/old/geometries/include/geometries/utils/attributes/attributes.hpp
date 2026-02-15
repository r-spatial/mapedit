#ifndef R_GEOMETRIES_UTILS_ATTRIBUTES_H
#define R_GEOMETRIES_UTILS_ATTRIBUTES_H

namespace geometries {
namespace utils {

  inline void attach_attributes( SEXP& obj, Rcpp::List& attributes ) {

    R_xlen_t k;
    R_xlen_t n_attributes = attributes.length();
    Rcpp::StringVector attr_names = attributes.names();
    for( k = 0; k < n_attributes; ++k ) {

      Rcpp::String cls = attr_names[ k ];
      Rcpp::StringVector vcls = {cls};
      Rcpp::StringVector attr = attributes[ k ];

      Rf_setAttrib( obj, vcls, attr );
    }
  }

  inline void attach_attributes( Rcpp::List& obj, Rcpp::List& attributes ) {

    R_xlen_t k;
    R_xlen_t n_attributes = attributes.length();
    Rcpp::StringVector attr_names = attributes.names();
    for( k = 0; k < n_attributes; ++k ) {

      Rcpp::String cls = attr_names[ k ];
      Rcpp::StringVector vcls = {cls};
      Rcpp::StringVector attr = attributes[ k ];

      Rf_setAttrib( obj, vcls, attr );
    }
  }

  template < int RTYPE >
  inline void attach_attributes( Rcpp::Matrix< RTYPE >& obj, Rcpp::List& attributes ) {

    R_xlen_t k;
    R_xlen_t n_attributes = attributes.length();
    Rcpp::StringVector attr_names = attributes.names();
    for( k = 0; k < n_attributes; ++k ) {

      Rcpp::String cls = attr_names[ k ];
      Rcpp::StringVector vcls = {cls};
      Rcpp::StringVector attr = attributes[ k ];

      Rf_setAttrib( obj, vcls, attr );
    }
  }

  template< int RTYPE >
  inline void attach_attributes( Rcpp::Vector< RTYPE >& obj, Rcpp::List& attributes ) {

    R_xlen_t k;
    R_xlen_t n_attributes = attributes.length();
    Rcpp::StringVector attr_names = attributes.names();
    for( k = 0; k < n_attributes; ++k ) {

      Rcpp::String cls = attr_names[ k ];
      Rcpp::StringVector vcls = {cls};
      Rcpp::StringVector attr = attributes[ k ];

      Rf_setAttrib( obj, vcls, attr );
    }
  }

} // utils
} // geometries

#endif
