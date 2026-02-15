#ifndef R_SFHEADERS_SF_MULTIPOINT_H
#define R_SFHEADERS_SF_MULTIPOINT_H

#include <Rcpp.h>
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/unique/unique.hpp"

#include "geometries/utils/rleid/rleid.hpp"

#include "sfheaders/sf/sf_utils.hpp"
#include "sfheaders/sfc/multipoint/sfc_multipoint.hpp"

namespace sfheaders {
namespace sf {

  inline SEXP sf_multipoint(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& multipoint_id,
      std::string xyzm
  ) {
    Rcpp::List sfc = sfheaders::sfc::sfc_multipoint( x, geometry_cols, multipoint_id, xyzm );

    SEXP ids = geometries::utils::get_ids( x, multipoint_id );
    sfheaders::sf::id_length_check( ids, sfc );

    Rcpp::DataFrame sf = sfheaders::sf::make_sf( sfc, ids );
    return sf;
  }

  inline SEXP sf_multipoint(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& multipoint_id,
      std::string xyzm,
      bool& keep
  ) {

    if( !keep ) {
      return sf_multipoint( x, geometry_cols, multipoint_id, xyzm );
    }

    Rcpp::List lst = geometries::utils::as_list( x );
    Rcpp::List sfc = sfheaders::sfc::sfc_multipoint( x, geometry_cols, multipoint_id, xyzm );

    SEXP property_cols = geometries::utils::other_columns( x, geometry_cols, multipoint_id );
    Rcpp::IntegerVector int_property_cols = geometries::utils::sexp_col_int( x, property_cols );

    if( Rf_isNull( multipoint_id ) ) {

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

    Rcpp::IntegerVector int_multipoint_id = geometries::utils::sexp_col_int( x, multipoint_id );

    // TODO:
    // - can I get this during 'sfc_linestring()' so I don't have to do it twice?
    Rcpp::IntegerVector geometry_idx = geometries::utils::rleid_indices( lst, int_multipoint_id );

    Rcpp::List res = Rcpp::List::create(
      Rcpp::_["x"] = lst,
      Rcpp::_["sfc"] = sfc,
      Rcpp::_["property_cols"] = int_property_cols,
      Rcpp::_["geometry_idx"] = geometry_idx,
      Rcpp::_["id_column"] = int_multipoint_id
    );

    return res;
  }

  inline SEXP sf_multipoint(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& multipoint_id
  ) {
    std::string xyzm;
    return sf_multipoint( x, geometry_cols, multipoint_id, xyzm );
  }

  inline SEXP sf_multipoint(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& multipoint_id,
      bool& keep
  ) {
    std::string xyzm;
    return sf_multipoint( x, geometry_cols, multipoint_id, xyzm, keep );
  }

} // sf
} // sfheaders

#endif
