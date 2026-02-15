#ifndef R_GEOMETRIES_TO_GEOMETRY_MATRIX_H
#define R_GEOMETRIES_TO_GEOMETRY_MATRIX_H

#include <Rcpp.h>
#include "geometries/utils/matrix/matrix.hpp"
#include "geometries/utils/utils.hpp"

namespace geometries {
namespace matrix {

  template< int RTYPE >
  inline SEXP to_geometry_matrix(
    Rcpp::Vector< RTYPE >& v
  ) {
    v.attr("dim") = Rcpp::Dimension( 1, v.length() );
    Rcpp::Matrix< RTYPE > m = Rcpp::as< Rcpp::Matrix< RTYPE > >( v );
    return m;
  }

  template< int RTYPE >
  inline SEXP to_geometry_matrix(
    Rcpp::Matrix< RTYPE >& m
  ) {
    return m;
  }

  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::DataFrame& df,
      bool keep_names = false
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = df.cols();
    R_xlen_t n_rows = df.rows();
    Rcpp::StringVector df_names = df.names();
    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {
      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( df[ i ] );
      nm( Rcpp::_, i ) = this_column;
    }

    if( keep_names ) {
      Rcpp::List m_attr(2);
      m_attr(1) = df_names;
      nm.attr("dimnames") = m_attr;
    }
    return nm;
  }

  // This assumes the list is 'square' and can
  // be directly converted into a matrix
  // i.e., it's a data.frame, but without the data.frame attributes
  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::List& lst,
      bool keep_names = false
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = Rf_length( lst );
    if( n_cols == 0 ) {
      Rcpp::stop("geometries - 0-length list found");
    }
    R_xlen_t n_rows = Rf_length( VECTOR_ELT( lst, 0 ) );

    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {
      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( lst[ i ] );
      nm( Rcpp::_, i ) = this_column;
    }

    if( keep_names ) {
      Rcpp::StringVector list_names = lst.names();
      Rcpp::List m_attr(2);
      m_attr(1) = list_names;
      nm.attr("dimnames") = m_attr;
    }
    return nm;
  }

  template< int RTYPE >
  inline SEXP to_geometry_matrix(
      Rcpp::Matrix< RTYPE >& m,
      Rcpp::IntegerVector& geometry_cols
  ) {
    geometries::utils::column_check( m, geometry_cols );
    R_xlen_t n_row = m.nrow();
    R_xlen_t n_col = geometry_cols.size();
    R_xlen_t i;
    Rcpp::Matrix< RTYPE > m2( n_row, n_col );
    for( i = 0; i < n_col; ++i ) {
      int this_col = geometry_cols[ i ];
      m2( Rcpp::_, i ) = m( Rcpp::_, this_col );
    }
    return m2;
  }

  template< int RTYPE >
  inline SEXP to_geometry_matrix(
    Rcpp::Vector< RTYPE >& v,
    Rcpp::IntegerVector& geometry_cols
  ) {
    geometries::utils::column_check( v, geometry_cols );
    R_xlen_t n_row = 1;
    R_xlen_t n_col = geometry_cols.size();
    R_xlen_t i;
    Rcpp::Matrix< RTYPE > m( n_row, n_col );

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    for( i = 0; i < n_col; ++i ) {
      int this_col = geometry_cols[ i ];
      T v2 = v[ this_col ];
      m( 0, i ) = v2;
    }
    return m;
  }

  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::DataFrame& df,
      Rcpp::StringVector& cols, // may only want a subset of columns
      bool keep_names = false
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = cols.size();
    R_xlen_t n_rows = df.rows();

    if( n_cols > df.ncol() ) {
      Rcpp::stop("geometries - number of columns requested is greater than those available");
    }
    Rcpp::StringVector df_names = df.names();
    Rcpp::StringVector m_names( n_cols );
    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {

      Rcpp::String this_col = cols[ i ];
      m_names[i] = this_col;
      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( df[ this_col ] );
      nm( Rcpp::_, i ) = this_column;
    }

    if( keep_names ) {
      Rcpp::List m_attr(2);
      m_attr(1) = m_names;
      nm.attr("dimnames") = m_attr;
    }

    return nm;
  }

  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::List& lst,
      Rcpp::StringVector& cols, // may only want a subset of columns
      bool keep_names = false
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = cols.size();
    if( Rf_length( lst ) == 0 ) {
      Rcpp::stop("geometries - 0-length list found");
    }
    R_xlen_t n_rows = Rf_length( VECTOR_ELT( lst, 0 ) );

    if( n_cols > Rf_length( lst ) ) {
      Rcpp::stop("geometries - number of columns requested is greater than those available");
    }

    Rcpp::StringVector m_names( n_cols );
    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {
      Rcpp::String this_col = cols[ i ];
      m_names[i] = this_col;
      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( lst[ this_col ] );
      nm( Rcpp::_, i ) = this_column;
    }

    if( keep_names ) {
      Rcpp::StringVector lst_names = lst.names();
      Rcpp::List m_attr(2);
      m_attr(1) = m_names;
      nm.attr("dimnames") = m_attr;
    }
    return nm;
  }

  template< int RTYPE >
  inline SEXP to_geometry_matrix(
    Rcpp::Matrix< RTYPE >& m,
    Rcpp::StringVector& geometry_cols
  ) {
    Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( m );
    return to_geometry_matrix( df, geometry_cols );
  }

  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::DataFrame& df,
      Rcpp::IntegerVector& cols, // may only want a subset of columns
      bool keep_names = false
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = cols.size();
    R_xlen_t n_rows = df.rows();

    if( n_cols > df.ncol() ) {
      Rcpp::stop("geometries - number of columns requested is greater than those available");
    }

    R_xlen_t max_idx = Rcpp::max( cols );

    if( max_idx > ( df.ncol() - 1 ) ) {
      Rcpp::stop("geometries - invalid column index");
    }

    Rcpp::StringVector df_names = df.names();
    Rcpp::StringVector m_names( n_cols );
    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {
      int this_col = cols[ i ];
      m_names[i] = df_names[ this_col ];
      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( df[ this_col ] );
      nm( Rcpp::_, i ) = this_column;
    }

    if( keep_names ) {
      Rcpp::List m_attr(2);
      m_attr(1) = m_names;
      nm.attr("dimnames") = m_attr;
    }
    return nm;
  }

  inline Rcpp::NumericMatrix to_geometry_matrix(
      Rcpp::List& lst,
      Rcpp::IntegerVector& cols // may only want a subset of columns
  ) {
    R_xlen_t i;
    R_xlen_t n_cols = cols.size();

    if( Rf_length( lst ) == 0 ) {
      Rcpp::stop("geometries - 0-length list found");
    }

    R_xlen_t n_rows = Rf_length( VECTOR_ELT(lst, 0) );

    if( n_cols > Rf_length( lst ) ) {
      Rcpp::stop("geometries - number of columns requested is greater than those available");
    }

    R_xlen_t max_idx = Rcpp::max( cols );

    if( max_idx > ( Rf_length( lst ) - 1 ) ) {
      Rcpp::stop("geometries - invalid column index");
    }

    Rcpp::NumericMatrix nm( n_rows, n_cols );

    for( i = 0; i < n_cols; ++i ) {
      int this_col = cols[ i ];

      Rcpp::NumericVector this_column = Rcpp::as< Rcpp::NumericVector >( lst[ this_col ] );
      nm( Rcpp::_, i ) = this_column;
    }

    return nm;
  }

  inline SEXP to_geometry_matrix(
      SEXP& x,
      Rcpp::StringVector& geometry_cols
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::stop("geometries - lines need to be matrices or data.frames");
        } else {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          return to_geometry_matrix( im, geometry_cols );
        }
      }
      case REALSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::stop("geometries - lines need to be matrices or data.frames");
        } else {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return to_geometry_matrix( nm, geometry_cols );
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame" ) ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          return to_geometry_matrix( df, geometry_cols );
        } else if (Rf_isNewList( x ) ) {
          Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
          return to_geometry_matrix( lst, geometry_cols );
        }// else default
      }
      default: {
        Rcpp::stop("geometries - lines need to be matrices or data.frames");
      }
    }
    return Rcpp::List::create(); // never reaches
  }

  inline SEXP to_geometry_matrix(
      SEXP& x,
      Rcpp::IntegerVector& geometry_cols
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
          return to_geometry_matrix( iv, geometry_cols );
        } else {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          return to_geometry_matrix( im, geometry_cols );
        }
      }
      case REALSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
          return to_geometry_matrix( nv, geometry_cols );
        } else {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return to_geometry_matrix( nm, geometry_cols );
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame" ) ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          return to_geometry_matrix( df, geometry_cols );

        } else if (Rf_isNewList( x ) ) {
          Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
          return to_geometry_matrix( lst, geometry_cols );
        }// else default
      }
      default: {
        Rcpp::stop("geometries - lines need to be matrices or data.frames");
      }
    }
    return Rcpp::List::create(); // never reaches
  }

  inline SEXP to_geometry_matrix(
      SEXP& x
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
          Rcpp::IntegerMatrix mat = to_geometry_matrix( iv );
          return mat;
        } else {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          return to_geometry_matrix( im );
        }
      }
      case REALSXP: {
        if( !Rf_isMatrix( x ) ) {
          Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
          Rcpp::NumericMatrix mat = to_geometry_matrix( nv );
          return mat;
        } else {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          return to_geometry_matrix( nm );
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame" ) ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          return to_geometry_matrix( df );

        } else if ( Rf_isNewList( x ) ) {
          Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
          return to_geometry_matrix( lst );
        }// else default
      }
      default: {
        Rcpp::stop("geometries - lines need to be matrices or data.frames");
      }
    }
    return Rcpp::List::create(); // never reaches
  }

  inline SEXP to_geometry_matrix(
      SEXP& x,
      SEXP& geometry_cols
  ) {
    if( Rf_isNull( geometry_cols ) ) {
      return to_geometry_matrix( x );
    }

    switch( TYPEOF( geometry_cols ) ) {
    case REALSXP: {}
    case INTSXP: {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( geometry_cols );
      if( iv.length() == 0 ) {
        return to_geometry_matrix( x );
      } else {
        return to_geometry_matrix( x, iv );
      }
    }
    case STRSXP: {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( geometry_cols );
      if( sv.length() == 0 ) {
        return to_geometry_matrix( x );
      } else {
        return to_geometry_matrix( x, sv );
      }
    }
    default: {
      Rcpp::stop("geometries - unknown column types");
    }
    }
  }

} // matrix
} // geometries


#endif
