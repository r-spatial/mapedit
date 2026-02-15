#ifndef R_GEOMETRIES_UTILS_UNIQUE_IDS_H
#define R_GEOMETRIES_UTILS_UNIQUE_IDS_H

#include <Rcpp.h>

#include "geometries/utils/unique/unique_sort.hpp"
#include "geometries/utils/sexp/sexp.hpp"

namespace geometries {
namespace utils {

  inline SEXP get_ids(
      SEXP& x,
      int& id_col
  ) {

    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    if( id_col < 0 || id_col >= n_col ) {
      Rcpp::stop("geometries - column index out of range");
    }

    switch( TYPEOF( x ) ) {
    case INTSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
        Rcpp::IntegerVector ids = im( Rcpp::_, id_col );
        SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
        return unique_ids;
      }
    }
    case REALSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
        Rcpp::NumericVector ids = nm( Rcpp::_, id_col );
        SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
        return unique_ids;
      }
    }
    case VECSXP: {
      if( Rf_inherits( x, "data.frame") ) {
        Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
        SEXP ids = df[ id_col ];
        SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
        return unique_ids;
      }
    }
      default: {
        Rcpp::stop("geometries - could not get id column");
      }
    }
    return Rcpp::List::create();
  }

  inline SEXP get_ids(
      SEXP& x,
      Rcpp::String& id_col
  ) {

    Rcpp::DataFrame df;

    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( Rf_isMatrix( x ) ) {
          df = Rcpp::as< Rcpp::DataFrame >( x );
          break;
          // Rcpp::IntegerVector ids = im( Rcpp::_, id_col );
          // SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
          // return unique_ids;
        }
      }
      case REALSXP: {
        if( Rf_isMatrix( x ) ) {
          df = Rcpp::as< Rcpp::DataFrame >( x );
          break;
          // Rcpp::NumericVector ids = nm( Rcpp::_, id_col );
          // SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
          // return unique_ids;
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame") ) {
          df = Rcpp::as< Rcpp::DataFrame >( x );
          break;
        }
      }
      default: {
        Rcpp::stop("geometries - could not get id column");
      }
    }

    SEXP ids = df[ id_col ];
    //Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( ids );

    SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
    return unique_ids;
  }

  // Returns the unique of the id column
  // if the column is null, 1L is returned
  inline SEXP get_ids(
      SEXP& x,
      SEXP& id_col
  ) {

    if( Rf_isNull( id_col ) ) {

      Rcpp::IntegerVector ids(1);
      ids[0] = 1;
      return ids;
    }

    switch( TYPEOF( id_col ) ) {
    // case REALSXP: {}
    case INTSXP: {
      Rcpp::IntegerVector iv_id_col = Rcpp::as< Rcpp::IntegerVector >( id_col );
      int i_id_col = iv_id_col[0];
      return get_ids( x, i_id_col );
    }
    case STRSXP: {
      Rcpp::StringVector sv_id_col = Rcpp::as< Rcpp::StringVector >( id_col );
      Rcpp::String s_id_col = sv_id_col[0];
      return get_ids( x, s_id_col );
    }
    default: {
      Rcpp::stop("geometries - can't determine id column type");
    }
    }
    return Rcpp::List::create();
  }

} // utils
} // geometries

#endif
