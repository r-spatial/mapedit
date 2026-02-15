#ifndef R_SFHEADERS_SF_H
#define R_SFHEADERS_SF_H

#include <Rcpp.h>
#include "sfheaders/sf/point/sf_point.hpp"
#include "sfheaders/sf/multipoint/sf_multipoint.hpp"
#include "sfheaders/sf/linestring/sf_linestring.hpp"
#include "sfheaders/sf/multilinestring/sf_multilinestring.hpp"
#include "sfheaders/sf/polygon/sf_polygon.hpp"
#include "sfheaders/sf/multipolygon/sf_multipolygon.hpp"

namespace sfheaders {
namespace api {

  // inline void get_list_column_index(
  //     SEXP& list_columns,
  //     Rcpp::IntegerVector& list_column_idx,
  //     SEXP& property_cols,  // only needs to be stringified IFF list_columns is a string?
  //     Rcpp::IntegerVector& property_idx
  // ) {
  //
  //   if( !Rf_isNull( list_columns ) ) {
  //     switch( TYPEOF( list_columns ) ) {
  //     case REALSXP: {}
  //     case INTSXP: {
  //       list_column_idx = Rcpp::as< Rcpp::IntegerVector >( list_columns );
  //       break;
  //     }
  //     case STRSXP: {
  //       Rcpp::StringVector str_list_columns = Rcpp::as< Rcpp::StringVector >( list_columns );
  //       Rcpp::StringVector str_property_cols = Rcpp::as< Rcpp::StringVector >( property_cols );
  //       Rcpp::IntegerVector idx = geometries::utils::where_is( str_list_columns, str_property_cols );
  //       list_column_idx = property_idx[ idx ];
  //       break;
  //
  //     }
  //     default:{
  //       Rcpp::stop("sfheaders - unknown list-column type");
  //     }
  //     }
  //   }
  // }

  inline SEXP to_sf(
    SEXP& obj,
    SEXP& geometry_columns,
    SEXP& multipoint_id,
    SEXP& linestring_id,
    SEXP& multilinestring_id,
    SEXP& polygon_id,
    SEXP& multipolygon_id,
    SEXP& list_columns,
    std::string xyzm,
    bool keep,
    bool close,
    const std::string sf_type
  ) {
      // TODO - Make the id_columns the correct type
      // then inside each if() condition, split it by polygon_id & linestring_id (for example)
      // assume the 0th index is the outer-most geometry
      // as this function will be called from R this is controllable.

    Rcpp::List sf_objs;

    // can I make the obj a data.frame here?
    // then can sort out all the column names as well

    // If numeric values are sent it; can I rely on them beign the same?
    // will the output 'df' remain un-sorted?
    // I think it will... right??....

    bool closed_attributes = Rf_length( list_columns ) > 0 && keep == true && close == true;

    if( sf_type == "POINT" ) {
      sf_objs = sfheaders::sf::sf_point( obj, geometry_columns, xyzm, keep );
    } else if ( sf_type == "MULTIPOINT" ) {
      sf_objs = sfheaders::sf::sf_multipoint( obj, geometry_columns, multipoint_id, xyzm, keep );
    } else if ( sf_type == "LINESTRING" ) {
      sf_objs = sfheaders::sf::sf_linestring( obj, geometry_columns, linestring_id, xyzm, keep );
    } else if ( sf_type == "MULTILINESTRING" ) {
      sf_objs = sfheaders::sf::sf_multilinestring( obj, geometry_columns, multilinestring_id, linestring_id, xyzm, keep );
    } else if ( sf_type == "POLYGON" ) {
      sf_objs = sfheaders::sf::sf_polygon( obj, geometry_columns, polygon_id, linestring_id, xyzm, keep, close, closed_attributes );
    } else if ( sf_type == "MULTIPOLYGON" ) {
      sf_objs = sfheaders::sf::sf_multipolygon( obj, geometry_columns, multipolygon_id, polygon_id, linestring_id, xyzm, keep, close, closed_attributes );
    } else {
      Rcpp::stop("sfheaders - unknown sf type");
    }

    // if sf_objs doesn't contain any elements, it means it went directly through make_sf()
    if( !sf_objs.containsElementNamed("x") ) {
      return sf_objs;
    }

    // otherwise, the returning sf_objs now contain all the same elements
    // (but some can be null)
    Rcpp::List data = sf_objs["x"];
    Rcpp::List sfc = sf_objs["sfc"];
    Rcpp::IntegerVector property_cols = sf_objs["property_cols"];
    Rcpp::IntegerVector geometry_idx = sf_objs["geometry_idx"];

    Rcpp::IntegerVector id_column;

    if( sf_objs.containsElementNamed("id_column") ) {
      id_column = sf_objs["id_column"];
    }

    Rcpp::IntegerVector int_list_columns;

    if( !Rf_isNull( list_columns ) ) {
      // need to use 'obj' here because 'list_columns' may be a string, but 'data'
      // has been made into an unnamed list
      int_list_columns = geometries::utils::sexp_col_int( obj, list_columns );
    }

    return sfheaders::sf::create_sf(data, sfc, id_column, property_cols, int_list_columns, geometry_idx );
 }

  // TODO
  // write each of the rcpp_sf_xxx functions here, so they appropriately call the to_sf() code
  inline SEXP sf_point( SEXP x, SEXP cols, std::string xyzm, bool keep ) {
    return to_sf( x, cols, R_NilValue, R_NilValue, R_NilValue, R_NilValue, R_NilValue, R_NilValue, xyzm, keep, false, "POINT" );
  }

  inline SEXP sf_multipoint( SEXP x, SEXP cols, SEXP multipoint_id, std::string xyzm, bool keep ) {
    return to_sf( x, cols, multipoint_id, R_NilValue, R_NilValue, R_NilValue, R_NilValue, R_NilValue, xyzm, keep, false, "MULTIPOINT" );
  }

  inline SEXP sf_linestring( SEXP x, SEXP cols, SEXP linestring_id, std::string xyzm, bool keep ) {
    return to_sf( x, cols, R_NilValue, linestring_id, R_NilValue, R_NilValue, R_NilValue, R_NilValue, xyzm, keep, false, "LINESTRING" );
  }

  inline SEXP sf_multilinestring( SEXP x, SEXP cols, SEXP multilinestring_id, SEXP linestring_id, std::string xyzm, bool keep ) {
    return to_sf( x, cols, R_NilValue, linestring_id, multilinestring_id, R_NilValue, R_NilValue, R_NilValue, xyzm, keep, false, "MULTILINESTRING" );
  }

  inline SEXP sf_polygon( SEXP x, SEXP cols, SEXP polygon_id, SEXP linestring_id, std::string xyzm, bool keep, bool close ) {
    return to_sf( x, cols, R_NilValue, linestring_id, R_NilValue, polygon_id, R_NilValue, R_NilValue, xyzm, keep, close, "POLYGON" );
  }

  inline SEXP sf_multipolygon( SEXP x, SEXP cols, SEXP multipolygon_id, SEXP polygon_id, SEXP linestring_id, std::string xyzm, bool keep, bool close ) {
    return to_sf( x, cols, R_NilValue, linestring_id, R_NilValue, polygon_id, multipolygon_id, R_NilValue, xyzm, keep, close, "MULTIPOLYGON" );
  }


} // api
} // sfheaders


#endif
