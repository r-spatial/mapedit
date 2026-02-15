#ifndef R_SFHEADERS_SFG_MULTIPOINT_H
#define R_SFHEADERS_SFG_MULTIPOINT_H

#include <Rcpp.h>

#include "sfheaders/sfg/sfg_types.hpp"
#include "sfheaders/utils/utils.hpp"

#include "geometries/utils/columns/columns.hpp"
#include "geometries/geometries.hpp"
#include "geometries/nest/nest.hpp"


namespace sfheaders {
namespace sfg {

  template< int RTYPE >
  inline SEXP sfg_multipoint(
      Rcpp::Matrix< RTYPE >& mat,
      std::string xyzm
  ) {
    sfheaders::sfg::make_sfg( mat, sfheaders::sfg::SFG_MULTIPOINT, xyzm );
    return mat;
  }

  template< int RTYPE >
  inline SEXP sfg_multipoint(
      Rcpp::Vector< RTYPE >& vec,
      std::string xyzm
  ) {
    R_xlen_t n = vec.length();
    Rcpp::Matrix< RTYPE > mat( 1, n );
    mat( 0, Rcpp::_ ) = vec;
    return sfg_multipoint( mat, xyzm );
  }

  inline SEXP sfg_multipoint(
      Rcpp::List& lst,
      std::string xyzm
  ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    sfheaders::sfg::make_sfg( lst2, sfheaders::sfg::SFG_MULTIPOINT, xyzm );
    return lst2;
  }

  inline SEXP sfg_multipoint(
      SEXP& x,
      SEXP& geometry_cols,
      std::string xyzm
  ) {

    //Rcpp::stop("stopping");
    SEXP geometry_mat = geometries::matrix::to_geometry_matrix( x, geometry_cols );
    R_xlen_t n_col = geometries::utils::sexp_n_col( geometry_mat );
    xyzm = sfheaders::utils::validate_xyzm( xyzm, n_col );
    sfheaders::sfg::make_sfg( geometry_mat, sfheaders::sfg::SFG_MULTIPOINT, xyzm );
    return geometry_mat;
  }

  inline SEXP sfg_multipoint(
      SEXP& x,
      std::string xyzm
  ) {
    SEXP geometry_cols = R_NilValue;
    return sfg_multipoint( x, geometry_cols, xyzm );
  }

  inline SEXP sfg_multipoint(
      SEXP& x
  ) {
    SEXP geometry_cols = R_NilValue;
    std::string xyzm;
    return sfg_multipoint( x, geometry_cols, xyzm );
  }

} // sfg
} // sfheaders


#endif
