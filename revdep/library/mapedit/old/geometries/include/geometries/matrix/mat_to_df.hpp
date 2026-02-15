#ifndef R_GEOMETRIES_SHAPES_MATRIX_TO_DF_H
#define R_GEOMETRIES_SHAPES_MATRIX_TO_DF_H

#include <Rcpp.h>
#include "geometries/utils/dataframe/dataframe.hpp"

namespace geometries {
namespace matrix {

  template < int RTYPE >
  inline Rcpp::DataFrame matrix_to_df( Rcpp::Matrix< RTYPE >& mat ) {

    Rcpp::StringVector names = Rcpp::colnames( mat );
    bool has_names = names.length() > 0;
    R_xlen_t n_col = mat.ncol();
    R_xlen_t n_row = mat.nrow();
    R_xlen_t i;
    Rcpp::List res( n_col );
    Rcpp::StringVector res_names( n_col );

    for( i = 0; i < n_col; ++i ) {
      res[ i ] = mat( Rcpp::_, i );
      if( !has_names ) {
        std::ostringstream os;
        os << "V" << i;
        res_names( i ) = os.str();
      } else {
        res_names( i ) = names( i );
      }
    }

    return geometries::utils::make_dataframe( res, n_row, res_names );

  }

} // matrix
} // geometries

#endif
