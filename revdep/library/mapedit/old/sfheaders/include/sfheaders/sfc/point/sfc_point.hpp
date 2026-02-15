#ifndef R_SFHEADERS_SFC_POINT_H
#define R_SFHEADERS_SFC_POINT_H

#include <Rcpp.h>
#include "sfheaders/sfc/sfc_attributes.hpp"
#include "sfheaders/sfc/sfc_types.hpp"
#include "sfheaders/sfc/zm_range.hpp"

#include "sfheaders/utils/utils.hpp"

#include "geometries/bbox/bbox.hpp"
#include "geometries/utils/columns/columns.hpp"
#include "geometries/geometries.hpp"

namespace sfheaders {
namespace sfc {

  inline SEXP sfc_point(
      SEXP& x,
      SEXP& geometry_cols,
      std::string xyzm
  ) {

    if( Rf_isNull( geometry_cols ) ) {
      // make this all the other columns, then send back in
      SEXP geometry_cols2 = geometries::utils::other_columns( x );
      return sfc_point( x, geometry_cols2, xyzm );
    }

    int n_empty = 0; // can have empty POINT objects
    int n_id_cols = 0;
    R_xlen_t col_counter = geometries::utils::sexp_length( geometry_cols );

    // After subset_geometries we have moved the geometry columns
    // into the 0:(n_geometry-1) positions
    xyzm = sfheaders::utils::validate_xyzm( xyzm, col_counter );

    Rcpp::StringVector class_attribute = { xyzm.c_str(), "POINT","sfg" };
    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = class_attribute
    );


    Rcpp::NumericVector bbox = sfheaders::bbox::start_bbox();
    Rcpp::NumericVector z_range = sfheaders::zm::start_z_range();
    Rcpp::NumericVector m_range = sfheaders::zm::start_m_range();
    geometries::bbox::calculate_bbox( bbox, x, geometry_cols );
    sfheaders::zm::calculate_zm_ranges( z_range, m_range, x, geometry_cols, xyzm );

    R_xlen_t required_cols = col_counter + n_id_cols;

    Rcpp::IntegerVector geometry_cols_int = geometries::utils::sexp_col_int( x, geometry_cols );

    Rcpp::List lst = geometries::utils::as_list( x );
    Rcpp::List res( required_cols );

    sfheaders::utils::subset_geometries( lst, res, geometry_cols_int );

    // POINTs need to be done slightly differnetly
    // because they can be NULLL
    //
    // return lst;

    Rcpp::List sfc = geometries::make_geometries( res, attributes, n_empty );

    return sfheaders::sfc::make_sfc( sfc, sfheaders::sfc::SFC_POINT, bbox, z_range, m_range, n_empty );

  }

  inline SEXP sfc_point(
      SEXP& x
  ) {
      SEXP geometry_cols = R_NilValue;
      std::string xyzm;
      return sfc_point( x, geometry_cols, xyzm );
  }


} // sfc
} // sfheaders

#endif
