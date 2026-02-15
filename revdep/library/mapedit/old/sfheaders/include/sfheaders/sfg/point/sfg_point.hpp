#ifndef R_SFHEADERS_SFG_POINT_H
#define R_SFHEADERS_SFG_POINT_H

#include <Rcpp.h>
#include "sfheaders/utils/utils.hpp"
#include "sfheaders/sfg/sfg_types.hpp"

#include "geometries/utils/columns/columns.hpp"
#include "geometries/geometries.hpp"
#include "geometries/nest/nest.hpp"

namespace sfheaders {
namespace sfg {

  template < int RTYPE >
  inline SEXP sfg_point(
    Rcpp::Vector< RTYPE >& vec,
    std::string xyzm
  ) {
    SEXP geometry_mat = geometries::matrix::to_geometry_matrix< RTYPE >( vec );
    sfheaders::sfg::make_sfg( geometry_mat, sfheaders::sfg::SFG_POINT, xyzm );
    return geometry_mat;
  }

  inline SEXP sfg_point(
      SEXP& x,
      SEXP& geometry_cols,
      std::string xyzm
  ) {

    SEXP geometry_mat = geometries::matrix::to_geometry_matrix( x, geometry_cols );
    R_xlen_t n_row = geometries::utils::sexp_n_row( geometry_mat );
    if( n_row > 1 ) {
      Rcpp::stop("sfheaders - points can only be one row");
    }
    R_xlen_t n_col = geometries::utils::sexp_n_col( geometry_mat );
    xyzm = sfheaders::utils::validate_xyzm( xyzm, n_col );
    sfheaders::sfg::make_sfg( geometry_mat, sfheaders::sfg::SFG_POINT, xyzm );
    return geometry_mat;
  }

  inline SEXP sfg_point(
      SEXP& x,
      std::string xyzm
  ) {
    SEXP geometry_cols = R_NilValue;
    return sfg_point( x, geometry_cols, xyzm );
  }

  inline SEXP sfg_point(
      SEXP& x
  ) {
    SEXP geometry_cols = R_NilValue;
    std::string xyzm;
    return sfg_point( x, geometry_cols, xyzm );
  }

} // sfg
} // sfheaders

#endif
