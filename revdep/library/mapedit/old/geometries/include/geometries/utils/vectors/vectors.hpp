#ifndef R_GEOMETRIES_UTILS_VECTORS_H
#define R_GEOMETRIES_UTILS_VECTORS_H

#include <Rcpp.h>
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/unique/unique_sort.hpp"

namespace geometries {
namespace utils {

  /*
   * Finds the position of values in a vector
   * -------
   * CAUTION
   * if no position is found it returns -1, which will need to be handled
   * by the calling function so you don't try and subset using -1 as the index
   * -------
   */
  inline int where_is( int to_find, Rcpp::IntegerVector& iv ) {
    int n = iv.size();
    int i;
    for( i = 0; i < n; ++i ) {
      if ( to_find == iv[i] ) {
        return i;
      }
    }
    //Rcpp::stop("geometries - could not find column");
    return -1; // #nocov
  }

  inline int where_is(
      Rcpp::String to_find,
      Rcpp::StringVector& sv ) {
    int n = sv.size();
    int i;
    for( i = 0; i < n; ++i ) {
      if ( to_find == sv[i] ) {
        return i;
      }
    }
    //Rcpp::stop("geometries - could not find column");
    return -1; // #nocov
  }

  inline Rcpp::IntegerVector where_is(
      Rcpp::IntegerVector& values_to_find,
      Rcpp::IntegerVector& vector_to_look_in
  ) {

    int n = values_to_find.size();
    int i;
    Rcpp::IntegerVector res( n );
    for ( i = 0; i < n; ++i ) {
      int to_find = values_to_find[ i ];
      res[ i ] = where_is( to_find, vector_to_look_in );
    }
    return res;
  }

  inline Rcpp::IntegerVector where_is(
      Rcpp::StringVector& values_to_find,
      Rcpp::StringVector& vector_to_look_in
  ) {

    int n = values_to_find.size();
    int i;
    Rcpp::IntegerVector res( n );
    for ( i = 0; i < n; ++i ) {
      Rcpp::String to_find = values_to_find[ i ];
      res[ i ] = where_is( to_find, vector_to_look_in );
    }
    return res;
  }

  inline Rcpp::IntegerVector where_is(
    SEXP& values_to_find,
    SEXP& x   // object, of which are names or indices
  ) {
    switch( TYPEOF( values_to_find ) ) {
      case REALSXP: {}
      case INTSXP: {
        Rcpp::IntegerVector values = Rcpp::as< Rcpp::IntegerVector >( values_to_find );
        Rcpp::IntegerVector look_in = geometries::utils::sexp_length( x );
        return where_is( values, look_in );
      }
      case STRSXP: {
        Rcpp::StringVector values = Rcpp::as< Rcpp::StringVector >( values_to_find );
        Rcpp::StringVector look_in = geometries::utils::sexp_col_names( x );
        return where_is( values, look_in );
      }
      default: {
        Rcpp::stop("geometries - error trying to find values in a vector");
      }
    }
    return Rcpp::IntegerVector::create(); // #nocov never reaches
  }

  inline SEXP concatenate_vectors(
    Rcpp::IntegerVector& iv_1,
    Rcpp::IntegerVector& iv_2
  ) {

    int n_1 = iv_1.length();
    int n_2 = iv_2.length();
    int n = n_1 + n_2;
    int i;

    Rcpp::IntegerVector iv( n );

    if( n_1 == 1 ) {
      iv[0] = iv_1[0];
    } else {
      for( i = 0; i < n_1; ++i ) {
        iv[i] = iv_1[i];
      }
    }

    if( n_2 == 1 ) {
      iv[ n_1 ] = iv_2[0];
    } else {
      int idx = 0;
      for( i = n_1; i < n; ++i ) {
        iv[i] = iv_2[ idx ];
        idx++;
      }
    }

    //if( sort_unique ) {
    //Rcpp::IntegerVector iv2 = Rcpp::sort_unique( iv );
    //return iv2;
    //}
    //return iv;
    return geometries::utils::get_sexp_unique( iv );
  }

  inline SEXP concatenate_vectors(
    Rcpp::IntegerVector& iv,
    int& i
  ) {
    Rcpp::IntegerVector iv2(1);
    iv2[0] = i;
    return concatenate_vectors(iv, iv2);
  }


  inline SEXP concatenate_vectors(
    Rcpp::NumericVector& nv_1,
    Rcpp::NumericVector& nv_2
  ) {

    int n_1 = nv_1.length();
    int n_2 = nv_2.length();
    int n = n_1 + n_2;
    int i;

    Rcpp::NumericVector nv( n );

    if( n_1 == 1 ) {
      nv[0] = nv_1[0];
    } else {
      for( i = 0; i < n_1; ++i ) {
        nv[i] = nv_1[i];
      }
    }

    if( n_2 == 1 ) {
      nv[ n_1 ] = nv_2[0];
    } else {
      int idx = 0;
      for( i = n_1; i < n; ++i ) {
        nv[i] = nv_2[ idx ];
        idx++;
      }
    }

    //if( sort_unique ) {
      // Rcpp::NumericVector nv2 = Rcpp::sort_unique( nv );
      // return nv2;
    //}
    //return nv;
    return geometries::utils::get_sexp_unique( nv );
  }

  inline SEXP concatenate_vectors(
    Rcpp::StringVector& sv_1,
    Rcpp::StringVector& sv_2
  ) {

    int n_1 = sv_1.length();
    int n_2 = sv_2.length();
    int n = n_1 + n_2;
    int i;

    Rcpp::StringVector sv( n );

    for( i = 0; i < n_1; ++i ) {
      sv[i] = sv_1[i];
    }

    int idx = 0;
    for( i = n_1; i < n; ++i ) {
      sv[i] = sv_2[ idx ];
      idx++;
    }

    return geometries::utils::get_sexp_unique( sv );

    //Rcpp::StringVector sv2 = Rcpp::sort_unique( sv );
    //Rcpp::StringVector sv2 = Rcpp::unique( sv );
    //return sv2;
    //return sv;
  }

  inline SEXP concatenate_vectors(
    Rcpp::StringVector& vec_1,
    Rcpp::String& vec_2
  ) {
    Rcpp::StringVector sv(1);
    sv[0] = vec_2.get_cstring();
    return concatenate_vectors( vec_1, sv );
  }

  inline SEXP concatenate_vectors(
    SEXP& vec_1,
    SEXP& vec_2
    // bool sort_unique = true
  ) {

    if( Rf_isNull( vec_1 ) && Rf_isNull( vec_2 ) ) {
      return R_NilValue;
    }

    if( Rf_isNull( vec_1 ) && !Rf_isNull( vec_2 ) ) {
      return vec_2;
    }

    if( !Rf_isNull( vec_1 ) && Rf_isNull( vec_2 ) ) {
      return vec_1;
    }

    if( TYPEOF( vec_1 ) != TYPEOF( vec_2 ) ) {
      Rcpp::stop("geometries - different vector types found");
    }

    // else - combine vec_1 & vec_2 into a single vector
    // int n_1 = geometries::utils::get_sexp_length( vec_1 );
    // int n_2 = geometries::utils::get_sexp_length( vec_2 );
    // int n = n_1 + n_2;
    // int i;

    switch(TYPEOF( vec_1 ) ) {
      case NILSXP: {
        return R_NilValue;
      }
      case LGLSXP: {}
      case INTSXP: {
        Rcpp::IntegerVector iv_1 = Rcpp::as< Rcpp::IntegerVector >( vec_1 );
        Rcpp::IntegerVector iv_2 = Rcpp::as< Rcpp::IntegerVector >( vec_2 );
        return concatenate_vectors( iv_1, iv_2 );
      }
      case REALSXP: {
        Rcpp::NumericVector nv_1 = Rcpp::as< Rcpp::NumericVector >( vec_1 );
        Rcpp::NumericVector nv_2 = Rcpp::as< Rcpp::NumericVector >( vec_2 );
        return concatenate_vectors( nv_1, nv_2 );
      }
      case STRSXP: {
        Rcpp::StringVector sv_1 = Rcpp::as< Rcpp::StringVector >( vec_1 );
        Rcpp::StringVector sv_2 = Rcpp::as< Rcpp::StringVector >( vec_2 );
        return concatenate_vectors( sv_1, sv_2 );
      }
      default: {
        Rcpp::stop("geometries - can't combine columns");
      }
    }

    return Rcpp::List::create(); // never reaches
  }

  /*
   * Expand Vector
   *
   *
   */
  inline void expand_vector(
      Rcpp::List& res,
      SEXP& v,
      Rcpp::IntegerVector& expanded_index,
      R_xlen_t& i
  ) {

    switch( TYPEOF( v ) ) {
      case LGLSXP: {
        Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( v );
        res[ i ] = lv[ expanded_index ];
        break;
      }
      case INTSXP: {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( v );
        res[ i ] = iv[ expanded_index ];
        break;
      }
      case REALSXP: {
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( v );
        Rcpp::NumericVector res_nv = nv[ expanded_index ];
        res[ i ] = res_nv;
        break;
      }
      case STRSXP: {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( v );
        res[ i ] = sv[ expanded_index ];
        break;
      }
      case CPLXSXP: {
        Rcpp::ComplexVector cv = Rcpp::as< Rcpp::ComplexVector >( v );
        res[ i ] = cv[ expanded_index ];
        break;
      }
      case RAWSXP: {
        Rcpp::RawVector rv = Rcpp::as< Rcpp::RawVector >( v );
        res[ i ] = rv[ expanded_index ];
        break;
      }
      case VECSXP: {
        Rcpp::List lst = Rcpp::as< Rcpp::List >( v );
        res[ i ] = lst[ expanded_index ];
        break;
      }
      default: {
        Rcpp::stop("geometries - unsupported column type when expanding vectors");
      }
    }
  }

  // For supporting >= 0.2.0
  inline void expand_vector(
      Rcpp::List& res,
      SEXP& v,
      Rcpp::NumericVector& expanded_index,
      R_xlen_t& i
  ) {
    Rcpp::stop("geometries - Expecting an integer vector for indexing");
  }


} // utils
} // geometries


#endif
