#ifndef R_SFHEADERS_BBOX_H
#define R_SFHEADERS_BBOX_H

#include <Rcpp.h>
#include "geometries/utils/utils.hpp"

namespace sfheaders {
namespace bbox {

  inline void attach_bbox_attributes( Rcpp::NumericVector& bbox ) {
    bbox.attr("class") = Rcpp::CharacterVector::create("bbox");
    bbox.attr("names") = Rcpp::CharacterVector::create("xmin", "ymin", "xmax", "ymax");
  }

  inline Rcpp::NumericVector start_bbox() {
    Rcpp::NumericVector bbox(4);  // xmin, ymin, xmax, ymax
    bbox(0) = bbox(1) = bbox(2) = bbox(3) = NA_REAL;
    return bbox;
  }

  // inline void get_bbox(
  //   Rcpp::NumericVector& bbox,
  //   SEXP sfg
  // ) {
  //   // iff it's a list, recurse
  //   if( Rf_isNewList( sfg ) ) {
  //     // round we go
  //     Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
  //     R_xlen_t i;
  //     for( i = 0; i < lst.size(); ++i ) {
  //       SEXP s = lst[ i ];
  //       get_bbox( bbox, s );
  //     }
  //
  //   } else {
  //     calculate_bbox( bbox, sfg );
  //   }
  // }
  //
  // // issue 59
  // // get_bbox for sfc (sfg) objects
  // inline void get_bbox(
  //   Rcpp::NumericVector& bbox,
  //   Rcpp::List& sfc
  // ) {
  //   // iterate over the sfc and re-calculate the bounding box
  //   //Rcpp::NumericVector bbox = start_bbox();
  //   R_xlen_t i;
  //   R_xlen_t n_sfgs = sfc.size();
  //   for( i = 0; i < n_sfgs; ++i ) {
  //     SEXP sfg = sfc[ i ];
  //     get_bbox( bbox, sfg );
  //   }
  //   attach_bbox_attributes( bbox );
  //   //return bbox;
  // }


} // bbox
} // sfheaders


#endif
