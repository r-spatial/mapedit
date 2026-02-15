#ifndef R_SFHEADERS_SFG_LINESTRING_H
#define R_SFHEADERS_SFG_LINESTRING_H

#include <Rcpp.h>
#include "sfheaders/sfg/sfg_types.hpp"
#include "sfheaders/utils/utils.hpp"

#include "geometries/utils/columns/columns.hpp"
#include "geometries/geometries.hpp"
#include "geometries/nest/nest.hpp"


namespace sfheaders {
namespace sfg {

  template< int RTYPE >
  inline SEXP sfg_linestring(
      Rcpp::Matrix< RTYPE >& mat,
      std::string xyzm
  ) {
    sfheaders::sfg::make_sfg( mat, sfheaders::sfg::SFG_LINESTRING, xyzm );
    return mat;
  }

  template< int RTYPE >
  inline SEXP sfg_linestring(
      Rcpp::Vector< RTYPE >& vec,
      std::string xyzm
  ) {
    R_xlen_t n = vec.length();
    Rcpp::Matrix< RTYPE > mat( 1, n );
    mat( 0, Rcpp::_ ) = vec;
    return sfg_linestring( mat, xyzm );
  }

  inline SEXP sfg_linestring(
      Rcpp::List& lst,
      std::string xyzm
  ) {
    sfheaders::sfg::make_sfg( lst, sfheaders::sfg::SFG_LINESTRING, xyzm );
    return lst;
  }

  inline SEXP sfg_linestring(
    SEXP& x,
    SEXP& geometry_cols,
    std::string xyzm
  ) {
    SEXP geometry_mat = geometries::matrix::to_geometry_matrix( x, geometry_cols );
    R_xlen_t n_col = geometries::utils::sexp_n_col( geometry_mat );
    xyzm = sfheaders::utils::validate_xyzm( xyzm, n_col );
    sfheaders::sfg::make_sfg( geometry_mat, n_col, sfheaders::sfg::SFG_LINESTRING, xyzm );
    return geometry_mat;
  }

  inline SEXP sfg_linestring(
      SEXP& x,
      std::string xyzm
  ) {
    SEXP geometry_cols = R_NilValue;
    return sfg_linestring( x, geometry_cols, xyzm );
  }

  inline SEXP sfg_linestring(
      SEXP& x
  ) {
    SEXP geometry_cols = R_NilValue;
    std::string xyzm;
    return sfg_linestring( x, geometry_cols, xyzm );
  }

} // sfg
} // sfheaders


#endif
