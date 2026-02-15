#ifndef R_GEOMETRIES_LIST_UTILS_H
#define R_GEOMETRIES_LIST_UTILS_H

#include <Rcpp.h>


namespace geometries {
namespace utils {

  template< int RTYPE >
  inline Rcpp::Vector< RTYPE > fill_vector(
      Rcpp::Vector< RTYPE >& vec_1,
      const Rcpp::Vector< RTYPE >& vec_2,
      R_xlen_t& start_idx
  ) {
    // fills vec_1 with vec_2, starting at 'start_idx'
    R_xlen_t i;
    R_xlen_t n = vec_2.length();

    for( i = 0; i < n; ++i ) {
      vec_1[ i + start_idx ] = vec_2[ i ] ;
    }
    return vec_1;
  }

  template< int RTYPE >
  inline Rcpp::List matrix_to_list(
      Rcpp::Matrix< RTYPE >& mat,
      R_xlen_t& total_rows
  ) {

    R_xlen_t n_col = mat.ncol();
    Rcpp::List res( n_col );
    R_xlen_t i;
    for( i = 0; i < n_col; ++i ) {
      res[ i ] = mat( Rcpp::_, i );
    }
    total_rows = mat.nrow();
    return res;
  }

  // this will put an 'id' directly onto the matrix
  template< int RTYPE >
  inline Rcpp::List matrix_to_list(
      Rcpp::Matrix< RTYPE >& mat,
      R_xlen_t& total_rows,
      double& id
  ) {

    R_xlen_t n_col = mat.ncol();
    R_xlen_t n_row = mat.nrow();
    Rcpp::Vector< RTYPE > id_vector = Rcpp::rep( id, n_row );
    Rcpp::List res( n_col + 1);  // for the id
    res[ 0 ] = id_vector;

    R_xlen_t i;

    for( i = 0; i < n_col; ++i ) {
      res[ i + 1 ] = mat( Rcpp::_, i );
    }
    total_rows = mat.nrow();
    return res;
  }

  template< int RTYPE >
  inline Rcpp::List vector_to_list(
      Rcpp::Vector< RTYPE >& v,
      R_xlen_t& total_rows
    ) {

    R_xlen_t n = v.length();
    Rcpp::List res( n );
    R_xlen_t i;
    for( i = 0; i < n; ++i ) {
      res[ i ] = v[ i ];
    }
    total_rows = 1; // TODO??
    return res;
  }

  template< int RTYPE >
  inline Rcpp::List vector_to_list(
      Rcpp::Vector< RTYPE >& v,
      R_xlen_t& total_rows,
      double& id
    ) {

    R_xlen_t n = v.length();
    Rcpp::List res( n + 1 );
    //Rcpp::NumericVector id_vector = Rcpp::seq( id, id + n );
    res[ 0 ] = id;
    ++id;
    R_xlen_t i;
    for( i = 0; i < n; ++i ) {
      res[ i + 1 ] = v[ i ];
    }
    total_rows = 1; // TODO??
    return res;
  }


} // utlis
} // geometries

#endif
