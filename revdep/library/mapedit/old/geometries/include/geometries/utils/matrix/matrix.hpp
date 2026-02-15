#ifndef R_GEOMETRIES_UTILS_MATRIX_H
#define R_GEOMETRIES_UTILS_MATRIX_H

#include <Rcpp.h>

namespace geometries {
namespace utils {

  inline Rcpp::StringVector matrix_names(
      Rcpp::List& m_attr
  ) {
    if( m_attr.size() < 2 ) {
      Rcpp::stop("geometries - could not find matrix names. Perhaps your matrix does not have names");
    }
    Rcpp::StringVector matrix_names = m_attr[1];
    return matrix_names;
  }

  template < int RTYPE >
  inline Rcpp::StringVector matrix_names(
      Rcpp::Matrix< RTYPE >& m
  ) {
    Rcpp::List m_attr = m.attr("dimnames");
    return matrix_names( m_attr );
  }


} // utils
} // geometries

#endif
