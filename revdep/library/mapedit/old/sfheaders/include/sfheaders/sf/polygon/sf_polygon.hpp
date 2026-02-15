#ifndef R_SFHEADERS_SF_POLYGON_H
#define R_SFHEADERS_SF_POLYGON_H

#include <Rcpp.h>
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/unique/unique.hpp"

#include "geometries/utils/rleid/rleid.hpp"

#include "sfheaders/sf/sf_utils.hpp"
#include "sfheaders/sfc/polygon/sfc_polygon.hpp"

namespace sfheaders {
namespace sf {

  // this function won't keep any attributes
  inline SEXP sf_polygon(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& polygon_id,
      SEXP& linestring_id,
      std::string xyzm,
      bool close,
      bool closed_attribute
  ) {

    Rcpp::List sfc = sfheaders::sfc::sfc_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close, closed_attribute );
    // TODO: we're getting the linestring_ids inside sfc_linestring,
    // and re-doing it here... say what...
    SEXP ids = geometries::utils::get_ids( x, polygon_id );
    sfheaders::sf::id_length_check( ids, sfc );

    Rcpp::DataFrame sf = sfheaders::sf::make_sf( sfc, ids );
    return sf;
  }

  inline SEXP sf_polygon(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& polygon_id,
      SEXP& linestring_id,
      std::string xyzm,
      bool keep,
      bool close,
      bool closed_attribute
  ) {

    if( !keep ) {
      return sf_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close, closed_attribute );
    }

    Rcpp::List lst = geometries::utils::as_list( x );
    Rcpp::List sfc = sfheaders::sfc::sfc_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close, closed_attribute );

    SEXP property_cols = geometries::utils::other_columns( x, geometry_cols, polygon_id, linestring_id );
    Rcpp::IntegerVector int_property_cols = geometries::utils::sexp_col_int( x, property_cols );

    if( Rf_isNull( polygon_id ) ) {

      Rcpp::IntegerVector geometry_index(1);
      geometry_index[0] = 0;

      return Rcpp::List::create(
        Rcpp::_["x"] = lst,
        Rcpp::_["sfc"] = sfc,
        Rcpp::_["property_cols"] = int_property_cols,
        Rcpp::_["geometry_idx"] = geometry_index
        // no id_column because it's null
      );
    }

    Rcpp::IntegerVector int_polygon_id = geometries::utils::sexp_col_int( x, polygon_id );
    Rcpp::IntegerVector geometry_idx = geometries::utils::rleid_indices( lst, int_polygon_id );

    Rcpp::List res = Rcpp::List::create(
      Rcpp::_["x"] = lst,
      Rcpp::_["sfc"] = sfc,
      Rcpp::_["property_cols"] = int_property_cols,
      Rcpp::_["geometry_idx"] = geometry_idx,
      Rcpp::_["id_column"] = int_polygon_id
    );

    return res;
  }

  inline SEXP sf_polygon(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& polygon_id,
      SEXP& linestring_id,
      bool close,
      bool closed_attribute
  ) {
    std::string xyzm;
    return sf_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, close, closed_attribute );
  }

  inline SEXP sf_polygon(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& polygon_id,
      SEXP& linestring_id,
      bool keep,
      bool close,
      bool closed_attribute
  ) {
    std::string xyzm;
    return sf_polygon( x, geometry_cols, polygon_id, linestring_id, xyzm, keep, close, closed_attribute );
  }

} // sf
} // sfheaders

#endif
