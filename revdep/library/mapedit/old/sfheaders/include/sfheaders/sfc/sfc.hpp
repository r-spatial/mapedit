#ifndef R_SFHEADERS_SFC_H
#define R_SFHEADERS_SFC_H

#include <Rcpp.h>
#include "sfheaders/sfc/point/sfc_point.hpp"
#include "sfheaders/sfc/point/sfc_points.hpp"
#include "sfheaders/sfc/multipoint/sfc_multipoint.hpp"
#include "sfheaders/sfc/multipoint/sfc_multipoints.hpp"
#include "sfheaders/sfc/linestring/sfc_linestring.hpp"
#include "sfheaders/sfc/linestring/sfc_linestrings.hpp"
#include "sfheaders/sfc/multilinestring/sfc_multilinestring.hpp"
#include "sfheaders/sfc/multilinestring/sfc_multilinestrings.hpp"
#include "sfheaders/sfc/polygon/sfc_polygon.hpp"
#include "sfheaders/sfc/polygon/sfc_polygons.hpp"
#include "sfheaders/sfc/multipolygon/sfc_multipolygon.hpp"
#include "sfheaders/sfc/multipolygon/sfc_multipolygons.hpp"

// #include "sfheaders/sfc/sfc_types.hpp"
// #include "geometries/bbox/bbox.hpp"
// #include "sfheaders/sfc/zm_range.hpp"

namespace sfheaders {
namespace sfc {

  // // inline SEXP to_sfc( SEXP& x, Rcpp::List params ) {
  // //
  // //   if( params.containsElementNamed("multipolygon_id") ) {
  // //     if( !params.containsElementNamed("polygon_id") || !params.containsElementNamed("line_id") ) {
  // //       Rcpp::stop("MULTIPOLYGONs require polygon_id and line_id arguments" );
  // //       // return to_multipolygon( ) ;
  // //     }
  // //   }
  // //
  // //   if( params.containsElementNamed("point_id") ) {
  // //     SEXP point_id = params["point_id"];
  // //     // can be a char giving column of data.frame, or numeric / int for column of matrix
  // //     switch( TYPEOF( point_id ) ) {
  // //     case INTSXP: {}
  // //     case REALSXP: {
  // //       // the parameters should be column indices, so int should be fine?
  // //       Rcpp::IntegerVector pt_id = Rcpp::as< Rcpp::IntegerVector >( point_id );
  // //       // if the point_id column has been supplied,
  // //       // we need to loop on those values and create sfc objects
  // //       // because sfg is a single object, not a collection?
  // //
  // //       //return sfheaders::sfg::sfc_multipoint( x, pt_id );
  // //     }
  // //     }
  // //   }
  // //
  // //   return Rcpp::List::create(); // never reaches
  // // }
  //
  //
  // inline SEXP to_sfc( SEXP& x, std::string geom_type, std::string xyzm ) {
  //   if( geom_type == "POINT" ) {
  //     return sfheaders::sfc::sfc_point( x, xyzm );
  //   } else if ( geom_type == "MULTIPOINT" ) {
  //     return sfheaders::sfc::sfc_multipoint( x, xyzm );
  //   } else if ( geom_type == "LINESTRING" ) {
  //     return sfheaders::sfc::sfc_linestring( x, xyzm );
  //   } else if ( geom_type == "MULTIILNESTRING" ) {
  //     return sfheaders::sfc::sfc_multilinestring( x, xyzm );
  //   } else if ( geom_type == "POLYGON" ) {
  //     return sfheaders::sfc::sfc_polygon( x, xyzm );
  //   } else if ( geom_type == "MULTIPOLYGON" ) {
  //     return sfheaders::sfc::sfc_multipolygon( x, xyzm );
  //   }
  //
  //   Rcpp::stop("sfheaders - unknown sfc geometry type");
  //   return Rcpp::List::create();
  // }
  //
  // inline SEXP to_sfc( SEXP& x, std::string geom_type, SEXP geometry_columns, std::string xyzm ) {
  //   if( geom_type == "POINT" ) {
  //     return sfheaders::sfc::sfc_point( x, geometry_columns, xyzm );
  //   } else if ( geom_type == "MULTIPOINT" ) {
  //     return sfheaders::sfc::sfc_multipoint( x, geometry_columns, xyzm );
  //   } else if ( geom_type == "LINESTRING" ) {
  //     return sfheaders::sfc::sfc_linestring( x, geometry_columns, xyzm );
  //   } else if ( geom_type == "MULTIILNESTRING" ) {
  //     return sfheaders::sfc::sfc_multilinestring( x, geometry_columns, xyzm );
  //   } else if ( geom_type == "POLYGON" ) {
  //     return sfheaders::sfc::sfc_polygon( x, geometry_columns, xyzm );
  //   } else if ( geom_type == "MULTIPOLYGON" ) {
  //     return sfheaders::sfc::sfc_multipolygon( x, geometry_columns, xyzm );
  //   }
  //
  //   Rcpp::stop("sfheaders - unknown sfc geometry type");
  //   return Rcpp::List::create(); // never reaches
  // }

} // sfc
} // sfheaders

#endif
