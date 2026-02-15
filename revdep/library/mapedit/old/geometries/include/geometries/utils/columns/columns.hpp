#ifndef R_GEOMETRIES_UTILS_COLUMNS_H
#define R_GEOMETRIES_UTILS_COLUMNS_H

#include <Rcpp.h>
#include "geometries/utils/matrix/matrix.hpp"
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/vectors/vectors.hpp"

namespace geometries {
namespace utils {

  template< int RTYPE >
  inline SEXP other_columns(
      Rcpp::Vector< RTYPE >& all_cols,
      Rcpp::Vector< RTYPE >& id_cols
  ) {

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    int n_id_cols = id_cols.size();
    int n_other_cols = all_cols.size();
    int i, j;
    bool is_in = false;

    for( i = 0; i < n_id_cols; ++i ) {
      is_in = false;
      T id_col = id_cols[i];
      for( j = 0; j < n_other_cols; ++j ) {
        T a_col = all_cols[j];
        if( id_col == a_col ) {
          // this column is one of the id ones, so we shouldn't keep it.
          is_in = true;
          break;
        }
      }
      if( is_in ) {
        all_cols.erase( j );
      }
    }
    return all_cols;
  }

  inline SEXP other_columns(
      Rcpp::DataFrame& df,
      Rcpp::StringVector& id_cols
  ) {
    Rcpp::StringVector df_names = df.names();
    return other_columns( df_names, id_cols );
  }

  inline SEXP other_columns(
      Rcpp::DataFrame& df,
      Rcpp::IntegerVector& id_cols
  ) {
    int n_col = df.ncol();
    Rcpp::IntegerVector other_cols = Rcpp::seq( 0, n_col - 1 );
    return other_columns( other_cols, id_cols );
  }

  template < int RTYPE >
  inline SEXP other_columns(
      Rcpp::Matrix< RTYPE >& m,
      Rcpp::StringVector& id_cols
  ) {
    Rcpp::StringVector m_names = geometries::utils::matrix_names( m );
    return other_columns( m_names, id_cols );
  }

  template < int RTYPE >
  inline SEXP other_columns(
      Rcpp::Matrix< RTYPE >& m,
      Rcpp::IntegerVector& id_cols
  ) {
    int n_col = m.ncol();
    Rcpp::IntegerVector other_cols = Rcpp::seq( 0, n_col - 1 );
    return other_columns( other_cols, id_cols );
  }

  inline SEXP other_columns(
      SEXP& x,
      Rcpp::IntegerVector& id_cols
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x ) ;
          return other_columns( im, id_cols );
        }
      }
      case REALSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return other_columns( nm, id_cols );
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame") ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          return other_columns( df, id_cols );
        }
      }
      default: {
        Rcpp::stop("geometries - unsupported object");
      }
    }
  }

  inline SEXP other_columns(
      SEXP& x
  ) {
    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    Rcpp::IntegerVector cols = Rcpp::seq( 0, (n_col - 1) );
    return cols;
  }

  inline SEXP other_columns(
      SEXP& x,
      Rcpp::StringVector& id_cols
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x ) ;
          return other_columns( im, id_cols );
        }
      }
      case REALSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return other_columns( nm, id_cols );
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame") ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          return other_columns( df, id_cols );
        }
      }
      default: {
        Rcpp::stop("geometries - unsupported object");
      }
    }
  }


  inline SEXP other_columns(
      SEXP& x,
      SEXP& id_cols // will be a vector
  ) {

    if( Rf_isNull( id_cols ) ) {
      return other_columns( x );
    }

    switch( TYPEOF( id_cols ) ) {
      // case REALSXP: {
      //   Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( id_cols );
      //   Rcpp::NumericVector nv2 = Rcpp::sort_unique( nv );
      //   return other_columns( x, nv2 );
      // }
      case INTSXP: {

        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( id_cols );
        Rcpp::IntegerVector iv2 = Rcpp::sort_unique( iv );
        return other_columns( x, iv2 );
      }
      case STRSXP: {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( id_cols );
        Rcpp::StringVector sv2 = Rcpp::sort_unique( sv );
        return other_columns( x, sv2 );
      }
      default: {
        Rcpp::stop("geometries - unsupported column types");
      }
    }
  }

  // when 2 columns are known
  inline SEXP other_columns(
    SEXP& x,
    SEXP& col_1,
    SEXP& col_2
  ) {

    if( Rf_isNull( col_1 ) && Rf_isNull( col_2 ) ) {
      return other_columns( x );
    }

    if( Rf_isNull( col_1 ) && !Rf_isNull( col_2 ) ) {
      return other_columns( x, col_2 );
    }

    if( !Rf_isNull( col_1 ) && Rf_isNull( col_2 ) ) {
      return other_columns( x, col_1 );
    }

    SEXP cols = geometries::utils::concatenate_vectors( col_1, col_2 );
    return other_columns( x, cols );

  }

  inline SEXP other_columns(
      SEXP& x,
      SEXP& col_1,
      SEXP& col_2,
      SEXP& col_3
  ) {

    if( !Rf_isNull( col_1 ) && Rf_isNull( col_2 ) && Rf_isNull( col_3 ) ) {
      return other_columns( x, col_1 );
    }

    if( Rf_isNull( col_1 ) && !Rf_isNull( col_2 ) && Rf_isNull( col_3 ) ) {
      return other_columns( x, col_2 );
    }

    if( Rf_isNull( col_1 ) && Rf_isNull( col_2 ) && !Rf_isNull( col_3 ) ) {
      return other_columns( x, col_3 );
    }

    if( Rf_isNull( col_1) && !Rf_isNull( col_2 ) && !Rf_isNull( col_3 ) ) {
      return other_columns( x, col_2, col_3 );
    }

    if ( !Rf_isNull( col_1 ) && Rf_isNull( col_2 ) && !Rf_isNull( col_3 ) ) {
      return other_columns( x, col_1, col_3 );
    }

    if ( !Rf_isNull( col_1 ) && !Rf_isNull( col_2 ) && Rf_isNull( col_3 ) ) {
      return other_columns( x, col_1, col_2 );
    }

    if ( Rf_isNull( col_1 ) && Rf_isNull( col_2 ) && Rf_isNull( col_3 ) ) {
      // then it's just all teh columns
      return other_columns( x );
    }

    // combine cols 1, 2 and 3
    SEXP other_cols_1 = geometries::utils::concatenate_vectors( col_1, col_2 );
    SEXP other_cols = geometries::utils::concatenate_vectors( other_cols_1, col_3 );
    return other_columns( x, other_cols );

  }

  template < int RTYPE >
  inline SEXP column_positions(
      Rcpp::Matrix< RTYPE >& m,
      Rcpp::StringVector& cols
  ) {

    Rcpp::StringVector m_names = geometries::utils::sexp_col_names( m );
    R_xlen_t n_col = cols.size();

    Rcpp::IntegerVector iv( n_col );
    R_xlen_t i;
    for( i = 0; i < n_col; ++i ) {
      Rcpp::String this_col = cols[i];
      iv[ i ] = geometries::utils::where_is( this_col, m_names );
    }
    return iv;
  }

  inline SEXP column_positions(
    SEXP& x,
    Rcpp::StringVector& cols
  ) {

    // if( !Rf_isMatrix( x ) ) {
    //   Rcpp::stop("geometries - expecting matrix when finding column positions");
    // }

    Rcpp::StringVector m_names = geometries::utils::sexp_col_names( x );
    R_xlen_t n_col = cols.size();

    Rcpp::IntegerVector iv( n_col );
    R_xlen_t i;
    for( i = 0; i < n_col; ++i ) {
      Rcpp::String this_col = cols[i];
      iv[ i ] = geometries::utils::where_is( this_col, m_names );
    }
    return iv;
  }

  inline SEXP column_positions(
    SEXP& x,
    SEXP& cols
  ) {

    if( !Rf_isVector( cols ) ) {
      Rcpp::stop("geometries - column indexes need to be a vector");
    }

    switch( TYPEOF( cols ) ) {
      case STRSXP: {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( cols );
        return column_positions( x, sv );
      }
      default: {
        Rcpp::stop("geometries - expecting string vector of column names when finding column positions");
      }
    }
    return Rcpp::List::create();
  }



} // utils
} // geometries

#endif
