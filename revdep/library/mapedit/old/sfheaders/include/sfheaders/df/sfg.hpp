#ifndef R_SFHEADERS_DF_SFG_H
#define R_SFHEADERS_DF_SFG_H

#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/lists/collapse.hpp"
#include "sfheaders/sfg/sfg_types.hpp"
#include "sfheaders/df/utils.hpp"

#include <Rcpp.h>

#define M_COLUMN                  12
#define Z_COLUMN                  11
#define Y_COLUMN                  10
#define X_COLUMN                  9
#define POINT_COLUMN              8
#define MULTIPOINT_COLUMN         7
#define LINESTRING_COLUMN         6
#define MULTILINESTRING_COLUMN    5
#define POLYGON_COLUMN            4
#define MULTIPOLYGON_COLUMN       3
#define GEOMETRYCOLLECTION_COLUMN 2
#define SFG_COLUMN                1
#define SFC_COLUMN                0

#define MAX_COLUMNS               13

namespace sfheaders {
namespace df {

  const Rcpp::CharacterVector column_names = {
    "sfc_id", "sfg_id", "geometrycollection_id", "multipolygon_id", "polygon_id", "multilinestring_id",
    "linestring_id", "multipoint_id", "point_id", "x","y","z","m"
  };

  inline Rcpp::CharacterVector make_names( Rcpp::CharacterVector& cls ) {

    std::string dim;
    std::string geometry;

    Rcpp::LogicalVector columns( column_names.length() );

    dim = cls[0];
    geometry = cls[1];

    if ( dim == "XYZM" ) {
      columns[ Z_COLUMN ] = true;
      columns[ M_COLUMN ] = true;
    } else if ( dim == "XYZ" ) {
      columns[ Z_COLUMN ] = true;
    } else if ( dim == "XYM" ) {
      columns[ M_COLUMN ] = true;
    }

    columns[ X_COLUMN ] = true;
    columns[ Y_COLUMN ] = true;

    if( geometry == "POINT" ) {
    } else if ( geometry == "MULTIPOINT" ) {

    } else if ( geometry == "LINESTRING" ) {

    } else if ( geometry == "MULTILINESTRING" ) {
      columns[ LINESTRING_COLUMN ] = true;

    } else if ( geometry == "POLYGON" ) {
      columns[ LINESTRING_COLUMN ] = true;

    } else if ( geometry == "MULTIPOLYGON" ) {
      columns[ LINESTRING_COLUMN ] = true;
      columns[ POLYGON_COLUMN ] = true;
    }
    return column_names[ columns ];
  }

  template < int RTYPE >
  inline Rcpp::List sfg_point_coordinates( Rcpp::Vector< RTYPE >& sfg, R_xlen_t& sfg_rows ) {
    Rcpp::List res = geometries::utils::vector_to_list( sfg, sfg_rows );
    return res;
  }

  inline Rcpp::List sfg_multipoint_coordinates( Rcpp::NumericMatrix& sfg, R_xlen_t& sfg_rows) {
    Rcpp::List res = geometries::utils::matrix_to_list( sfg, sfg_rows );
    return res;
  }

  inline Rcpp::List sfg_linestring_coordinates( Rcpp::NumericMatrix& sfg, R_xlen_t& sfg_rows ) {
    Rcpp::List res = geometries::utils::matrix_to_list( sfg, sfg_rows );
    return res;
  }

  inline Rcpp::List sfg_multilinestring_coordinates( Rcpp::List& sfg, R_xlen_t& sfg_rows ) {
    R_xlen_t i;
    R_xlen_t n = sfg.size();
    Rcpp::List res( n );
    R_xlen_t total_rows = 0;
    for( i = 0; i < n; ++i ) {
      Rcpp::NumericMatrix mat = sfg[ i ];
      total_rows = total_rows + mat.nrow();
      res[ i ] = geometries::utils::matrix_to_list( mat, sfg_rows );
    }
    sfg_rows = total_rows;
    res = geometries::utils::collapse_list( res, total_rows );
    return res;
  }

  inline Rcpp::List sfg_polygon_coordinates( Rcpp::List& sfg, R_xlen_t& sfg_rows ) {
    return sfg_multilinestring_coordinates( sfg, sfg_rows );
  }

  inline Rcpp::List sfg_multipolygon_coordinates( Rcpp::List& sfg, R_xlen_t& sfg_rows ) {
    R_xlen_t i;

    R_xlen_t n = sfg.size();
    Rcpp::List res( n );
    R_xlen_t total_rows = 0;
    R_xlen_t inner_total_rows;

    sfheaders::utils::getSfgClass( sfg );

    for( i = 0; i < n; ++i ) {
      Rcpp::List lst = sfg[ i ];

      R_xlen_t inner_n = lst.size();
      inner_total_rows = 0;
      Rcpp::List inner_res( inner_n );

      res[ i ] = sfg_polygon_coordinates( lst, inner_total_rows );
      total_rows = total_rows + inner_total_rows;
    }

    sfg_rows = total_rows;
    res = geometries::utils::collapse_list( res, total_rows );
    return res;
  }

  inline Rcpp::List sfg_to_df( SEXP& sfg ) {

    Rcpp::List res;

    Rcpp::CharacterVector cls = sfheaders::utils::getSfgClass( sfg );

    //std::string dim;
    std::string geometry;

    Rcpp::LogicalVector columns( column_names.length() );

    //dim = cls[0];
    geometry = cls[1];

    R_xlen_t sfg_rows = 0;

    if( geometry == "POINT" ) {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sfg );
      res = sfg_point_coordinates( nv, sfg_rows );

    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      res = sfg_multipoint_coordinates( nm, sfg_rows );

    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      res = sfg_linestring_coordinates( nm, sfg_rows );

    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      res = sfg_multilinestring_coordinates( lst, sfg_rows );

    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      res = sfg_polygon_coordinates( lst, sfg_rows );

    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      res = sfg_multipolygon_coordinates( lst, sfg_rows );

    } else {
      Rcpp::stop("sfheaders - unknown geometry type"); // #nocov
    }

    Rcpp::CharacterVector df_names = make_names( cls );

    res.attr("class") = Rcpp::CharacterVector("data.frame");

    if( sfg_rows > 0 ) {
      Rcpp::IntegerVector rownames = Rcpp::seq( 1, sfg_rows );
      res.attr("row.names") = rownames;
    } else {
      res.attr("row.names") = Rcpp::IntegerVector(0); // #nocov
    }

    res.attr("names") = df_names;
    return res;
  }

} // df
} // sfheaders

#endif
