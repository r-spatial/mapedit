#ifndef R_SFHEADERS_SFC_TYPES_H
#define R_SFHEADERS_SFC_TYPES_H

#include <Rcpp.h>

#include "sfheaders/sfc/sfc_attributes.hpp"

namespace sfheaders {
namespace sfc {

  inline std::string get_sfc_type( int sfc_type ) {
    switch( sfc_type ) {
    case SFC_POINT: { return "POINT"; }
    case SFC_MULTIPOINT: { return "MULTIPOINT"; }
    case SFC_LINESTRING: { return "LINESTRING"; }
    case SFC_MULTILINESTRING: { return "MULTILINESTRING"; }
    case SFC_POLYGON: { return "POLYGON"; }
    case SFC_MULTIPOLYGON: { return "MULTIPOLYGON"; }
    default: {
      Rcpp::stop("sfheaders - unknown sfc type");     // #nocov
    }
    }
    return ""; // never reaches
  }

  inline SEXP make_sfc(
    Rcpp::List& sfc,
    int sfc_type,
    Rcpp::NumericVector& bbox,
    Rcpp::NumericVector& z_range,
    Rcpp::NumericVector& m_range,
    int n_empty = 0
  ) {

    std::string geom = get_sfc_type( sfc_type );
    std::unordered_set< std::string > geometry_types{ geom };

    Rcpp::String crs_input = NA_STRING;
    Rcpp::String crs_wkt = NA_STRING;

    Rcpp::List crs = Rcpp::List::create(
      Rcpp::_["input"] = crs_input,
      Rcpp::_["wkt"] = crs_wkt
    );
    //int n_empty = 0;
    double precision = 0.0;

    return sfheaders::sfc::create_sfc(
      sfc, geom, geometry_types, bbox, z_range, m_range, crs, n_empty, precision
      );
  }

  // inline SEXP make_sfc(
  //     Rcpp::IntegerVector& im,
  //     int sfc_type
  // ) {
  //
  // }
  //
  // inline SEXP make_sfc(
  //     Rcpp::NumericVector& nm,
  //     int sfc_type
  // ) {
  //
  // }
  //
  // inline SEXP make_sfc(
  //   Rcpp::IntegerMatrix& im,
  //   int sfc_type
  // ) {
  //
  // }
  //
  // inline SEXP make_sfc(
  //   Rcpp::NumericMatrix& nm,
  //   int sfc_type
  // ) {
  //
  // }


  inline SEXP make_sfc(
      SEXP& x,
      int sfc_type,
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& z_range,
      Rcpp::NumericVector& m_range
  ) {

    // std::string geom = get_sfc_type( sfc_type );
    // std::unordered_set< std::string > geometry_types{ geom };
    //
    // Rcpp::String epsg = NA_STRING;
    // Rcpp::String proj4string = NA_STRING;
    // int n_empty = 0;
    // double precision = 0.0;

    switch( TYPEOF( x ) ) {
    // case INTSXP: {
    // if( Rf_isMatrix( x ) ) {
    //   Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
    //   return make_sfc( im, sfc_type );
    // } else {
    //   Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
    //   return make_sfc( iv, sfc_type );
    // }
    // case REALSXP: {
    // if( Rf_isMatrix( x ) ) {
    //   Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
    //   return make_sfc( nm, sfc_type );
    // } else {
    //   Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
    //   return make_sfc( nv, sfc_type );
    // }
    // }
    case VECSXP: {
      Rcpp::List lst = Rcpp::as< Rcpp::List > ( x );
      return make_sfc( lst, sfc_type, bbox, z_range, m_range );
    }
    default: {
      Rcpp::stop("sfheaders - unexpected sfc type");
    }
    }
  }


} // sfg
} // sfheaders


#endif
