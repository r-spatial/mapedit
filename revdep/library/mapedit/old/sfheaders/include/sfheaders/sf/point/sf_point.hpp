#ifndef R_SFHEADERS_SF_POINT_H
#define R_SFHEADERS_SF_POINT_H

#include <Rcpp.h>
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/unique/unique.hpp"

#include "geometries/utils/rleid/rleid.hpp"

#include "sfheaders/sf/sf_utils.hpp"
#include "sfheaders/sfc/point/sfc_point.hpp"

namespace sfheaders {
namespace sf {

  inline SEXP sf_point(
      SEXP& x,
      SEXP& geometry_cols,
      std::string xyzm
  ) {

    Rcpp::List sfc = sfheaders::sfc::sfc_point( x, geometry_cols, xyzm );
    Rcpp::DataFrame sf = sfheaders::sf::make_sf( sfc );
    return sf;
  }

  inline SEXP sf_point(
      SEXP& x,
      SEXP& geometry_cols,
      std::string xyzm,
      bool& keep
  ) {

    if( !keep ) {
      return sf_point( x, geometry_cols, xyzm );
    }

    Rcpp::List lst = geometries::utils::as_list( x );
    Rcpp::List sfc = sfheaders::sfc::sfc_point( x, geometry_cols, xyzm );

    SEXP property_cols = geometries::utils::other_columns( x, geometry_cols );
    Rcpp::IntegerVector int_property_cols = geometries::utils::sexp_col_int( x, property_cols );

    R_xlen_t n_row = Rf_length( VECTOR_ELT( lst, 0 ) );
    Rcpp::IntegerVector geometry_index = Rcpp::seq( 0, n_row - 1 );

    return Rcpp::List::create(
      Rcpp::_["x"] = lst,
      Rcpp::_["sfc"] = sfc,
      Rcpp::_["property_cols"] = int_property_cols,
      Rcpp::_["geometry_idx"] = geometry_index
      // no id_column because it's null
    );
  }

  inline SEXP sf_point(
      SEXP& x,
      SEXP& geometry_cols
  ) {
    std::string xyzm;
    return sf_point( x, geometry_cols, xyzm );
  }

  inline SEXP sf_point(
      SEXP& x,
      SEXP& geometry_cols,
      bool& keep
  ) {
    std::string xyzm;
    return sf_point( x, geometry_cols, xyzm, keep );
  }

} // sf
} // sfheaders

#endif
