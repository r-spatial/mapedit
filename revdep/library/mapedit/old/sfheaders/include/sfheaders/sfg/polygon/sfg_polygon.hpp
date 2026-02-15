#ifndef R_SFHEADERS_SFG_POLYGON_H
#define R_SFHEADERS_SFG_POLYGON_H

#include <Rcpp.h>
#include "sfheaders/sfg/sfg_types.hpp"
#include "sfheaders/utils/utils.hpp"
#include "sfheaders/sfg/polygon/close_polygon.hpp"

#include "geometries/utils/utils.hpp"
#include "geometries/geometries.hpp"

namespace sfheaders {
namespace sfg {

  template< int RTYPE >
  inline SEXP sfg_polygon(
      Rcpp::Matrix< RTYPE >& mat,
      std::string xyzm,
      bool close = true
  ) {
    Rcpp::List p( 1 );
    p[0] = sfheaders::polygon_utils::close_polygon< RTYPE >( mat, close );
    R_xlen_t n_col = mat.ncol();

    sfheaders::sfg::make_sfg( p, n_col, sfheaders::sfg::SFG_POLYGON, xyzm );
    return p;
  }

  inline SEXP sfg_polygon(
      Rcpp::List& lst,
      std::string xyzm,
      bool close = true
  ) {
    lst = sfheaders::polygon_utils::close_polygon( lst, close );
    sfheaders::sfg::make_sfg( lst, sfheaders::sfg::SFG_POLYGON, xyzm );
    return lst;
  }

  inline SEXP sfg_polygon(
      SEXP& x,
      SEXP& geometry_cols,
      SEXP& linestring_id,
      std::string xyzm,
      bool close = true
  ) {

    if( !Rf_inherits( x, "data.frame") && Rf_isNewList( x ) ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
      return sfg_polygon( lst, xyzm, close );
    }

    // needs to go through the make_geoemtries()
    // but attributes are assigned at the end

    if( Rf_isNull( geometry_cols ) ) {
      // make this all the other columns, then send back in
      SEXP geometry_cols2 = geometries::utils::other_columns( x, linestring_id );
      return sfg_polygon( x, geometry_cols2, linestring_id, xyzm, close );
    }

    int n_id_cols = 1;
    R_xlen_t col_counter = geometries::utils::sexp_length( geometry_cols );

    // After subset_geometries we have moved the geometry columns
    // into the 0:(n_geometry-1) positions
    Rcpp::IntegerVector int_geometry_cols = Rcpp::seq( 0, ( col_counter - 1 ) );

    xyzm = sfheaders::utils::validate_xyzm( xyzm, col_counter );

    R_xlen_t required_cols = col_counter + n_id_cols;

    Rcpp::IntegerVector geometry_cols_int = geometries::utils::sexp_col_int( x, geometry_cols );

    Rcpp::List lst = geometries::utils::as_list( x );
    Rcpp::List res( required_cols );

    sfheaders::utils::subset_geometries( lst, res, geometry_cols_int );

    Rcpp::IntegerVector int_linestring_id(1);

    sfheaders::utils::resolve_id( x, linestring_id, int_linestring_id, res, lst, col_counter );

    Rcpp::List attributes = Rcpp::List::create();
    Rcpp::List sfg = geometries::make_geometries( res, int_linestring_id, int_geometry_cols, attributes, close );

    Rcpp::StringVector class_attribute = { xyzm.c_str(), "POLYGON","sfg" };
    Rcpp::List atts = Rcpp::List::create(
      Rcpp::_["class"] = class_attribute
    );
    geometries::utils::attach_attributes( sfg, atts );

    return sfg;

  }

  inline SEXP sfg_polygon(
      SEXP& sfg_poly,
      std::string xyzm,
      bool close = true
  ) {
    SEXP geometry_cols = R_NilValue;
    SEXP id_cols = R_NilValue;
    return sfg_polygon( sfg_poly, geometry_cols, id_cols, xyzm, close );
  }

  inline SEXP sfg_polygon(
      SEXP& sfg_poly,
      std::string xyzm
  ) {
    SEXP geometry_cols = R_NilValue;
    SEXP id_cols = R_NilValue;
    bool close = true;
    return sfg_polygon( sfg_poly, geometry_cols, id_cols, xyzm, close );
  }

  inline SEXP sfg_polygon(
      SEXP& sfg_poly,
      bool close = true
  ) {
    SEXP geometry_cols = R_NilValue;
    SEXP id_cols = R_NilValue;
    std::string xyzm;
    return sfg_polygon( sfg_poly, geometry_cols, id_cols, xyzm, close );
  }

  inline SEXP sfg_polygon(
      SEXP& sfg_poly
  ) {
    SEXP geometry_cols = R_NilValue;
    SEXP id_cols = R_NilValue;
    std::string xyzm;
    bool close = true;
    return sfg_polygon( sfg_poly, geometry_cols, id_cols, xyzm, close );
  }


  inline SEXP remove_polygon_holes(
      Rcpp::List& sfg_poly,
      std::string xyzm,
      bool close = true
  ) {
    SEXP res = sfg_poly[0];
    return sfg_polygon( res, xyzm, close );
  }


  // sfg_box will convert a bounding-box 4-value vector into a polygon
  template< int RTYPE >
  inline SEXP sfg_box( Rcpp::Vector< RTYPE > x ) {
    if ( x.length() != 4 ) {
      Rcpp::stop("sfheaders - box requires a 4-value vector");
    }

    Rcpp::Matrix< RTYPE > mat( 5, 2 );
    Rcpp::IntegerVector bl = {0,1};
    Rcpp::IntegerVector br = {2,1};
    Rcpp::IntegerVector tr = {2,3};
    Rcpp::IntegerVector tl = {0,3};

    Rcpp::Vector< RTYPE > blv = x[ bl ];
    Rcpp::Vector< RTYPE > brv = x[ br ];
    Rcpp::Vector< RTYPE > trv = x[ tr ];
    Rcpp::Vector< RTYPE > tlv = x[ tl ];

    mat( 0, Rcpp::_ ) = blv;
    mat( 1, Rcpp::_ ) = brv;
    mat( 2, Rcpp::_ ) = trv;
    mat( 3, Rcpp::_ ) = tlv;
    mat( 4, Rcpp::_ ) = blv;

    // Rcpp::StringVector class_attribute = { "XY", "POLYGON","sfg" };
    // Rcpp::List atts = Rcpp::List::create(
    //   Rcpp::_["class"] = class_attribute
    // );

    //geometries::utils::attach_attributes( mat, atts );

    std::string xyzm = "XY";
    bool close = false;
    return sfg_polygon( mat, xyzm, close );
  }

} // sfg
} // sfheaders

#endif
