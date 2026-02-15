
#ifndef R_GEOMETRIES_UTILS_SEXP_H
#define R_GEOMETRIES_UTILS_SEXP_H

#include <Rcpp.h>

namespace geometries{
namespace utils {

  inline R_xlen_t has_been_closed_attribute( SEXP& x ) {
    Rcpp::StringVector attr(1);
    attr[0] = "closed";
    SEXP a = Rf_getAttrib( x, attr );
    if( !Rf_isNull( a ) ) {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( a );
      Rcpp::String s = sv[0];
      const char* cs = s.get_cstring();

      return strcmp( cs, "has_been_closed" ) == 0 ? 1 : 0;
    }
    return 0;
  }

  inline Rcpp::StringVector name_attributes( SEXP& x ) {
      Rcpp::StringVector attr(1);
      attr[0] = "names";
      SEXP a = Rf_getAttrib( x, attr );
      if( !Rf_isNull( a ) ) {
        return Rcpp::as< Rcpp::StringVector >( a );
      }

    Rcpp::stop("geometries - object does not have names");
  }

  template< int RTYPE >
  inline Rcpp::StringVector sexp_col_names( Rcpp::Matrix< RTYPE >& mat ) {
    return colnames( mat );
  }

  inline Rcpp::StringVector sexp_col_names( SEXP& x ) {
    if( Rf_isMatrix( x ) ) {
      return Rcpp::colnames( x );
    }
    return name_attributes( x );
  }

  inline R_xlen_t sexp_n_col( SEXP& x ) {

    if( Rf_isMatrix( x ) ) {
      return Rf_ncols( x );
    }

    // if( Rf_isNewList( x ) || Rf_inherits( x, "data.frame") ) {
    //   return Rf_length( x );
    // }
    return Rf_length( x );
  }

  inline R_xlen_t sexp_n_row( SEXP& x ) {

    if( Rf_isNewList( x ) || Rf_inherits( x, "data.frame" ) ) {
      if( Rf_length( x ) == 0 ) {
        return 0;
      } else {
        return Rf_length( VECTOR_ELT( x, 0 ) );
      }
    }

    if( !Rf_isMatrix( x ) && Rf_isVector( x ) ) {
      return 1;
    }
    return Rf_nrows( x );
  }

  template < int RTYPE >
  inline R_xlen_t sexp_length( Rcpp::Vector< RTYPE >& v ) {
    return v.length();
  }

  inline R_xlen_t sexp_length( SEXP& s ) {

    return Rf_length( s );
    // switch( TYPEOF(s) ) {
    // case LGLSXP:
    //   return sexp_length< LGLSXP >( s );
    // case REALSXP:
    //   return sexp_length< REALSXP >( s );
    // case VECSXP:
    //   return sexp_length< VECSXP >( s );
    // case INTSXP:
    //   return sexp_length< INTSXP >( s );
    // case STRSXP:
    //   return sexp_length< STRSXP >( s );
    // default: Rcpp::stop("geometries - unknown vector type");
    // }
    // return 0;
  }

  // finds the integer index position of column names
  inline Rcpp::IntegerVector sexp_col_int(
      Rcpp::StringVector& names,
      std::string& s
  ) {
    Rcpp::IntegerVector ians( 1 );

    R_xlen_t i;
    for( i = 0; i < names.length(); ++i ) {
      const char * n = names[ i ];
      if( strcmp( s.c_str(), n ) == 0) {
        ians[0] = i;
        break;
      }
    }
    return ians;
  }

  inline Rcpp::IntegerVector sexp_col_int(
      Rcpp::StringVector& names,
      Rcpp::StringVector& s
  ) {

    Rcpp::IntegerVector ians( s.length() );

    R_xlen_t i, j;
    for(i = 0; i < s.length(); ++i ) {
      const char * id = s[ i ];
      for(j = 0; j < names.length(); ++j ) {
        const char * n = names[ j ];
        if( strcmp( id, n ) == 0 ) {
          ians[ i ] = j;
          break;
        }
      }
    }
    return ians;
  }

  inline Rcpp::IntegerVector sexp_col_int(
    Rcpp::DataFrame& df,
    SEXP& v
  ) {
    switch( TYPEOF( v ) ) {
      case INTSXP: {
        return Rcpp::as< Rcpp::IntegerVector >( v );
      }
      case STRSXP: {
        Rcpp::StringVector s = Rcpp::as< Rcpp::StringVector >( v );
        Rcpp::StringVector df_names = df.names();
        return sexp_col_int( df_names, s );
      }
      default: {
        Rcpp::stop("geometries - require either integer or string column indices");
      }
    }
  }

  inline Rcpp::IntegerVector sexp_col_int(
      Rcpp::List& lst,
      SEXP& v
  ) {
    switch( TYPEOF( v ) ) {
      case INTSXP: {
        return Rcpp::as< Rcpp::IntegerVector >( v );
      }
      case STRSXP: {
        Rcpp::StringVector s = Rcpp::as< Rcpp::StringVector >( v );
        Rcpp::StringVector lst_names = lst.names();
        return sexp_col_int( lst_names, s );
      }
      default: {
        Rcpp::stop("geometries - require either integer or string column indices");
      }
    }
  }

  inline Rcpp::IntegerVector sexp_col_int(
      SEXP& x,
      Rcpp::StringVector& s
  ) {
    Rcpp::StringVector names = geometries::utils::sexp_col_names( x );
    return sexp_col_int( names, s );
  }

  inline Rcpp::IntegerVector sexp_col_int(
      SEXP& x,
      SEXP& v
  ) {

    switch( TYPEOF( v ) ) {
      case INTSXP: {
        return Rcpp::as< Rcpp::IntegerVector >( v );
      }
      case STRSXP: {
        Rcpp::StringVector s = Rcpp::as< Rcpp::StringVector >( v );
        return sexp_col_int( x, s );
      }
      default: {
        Rcpp::stop("geometries - require either integer or string column indices");
      }
    }
    return Rcpp::IntegerVector(0);
  }

} // utils
} // geometries




#endif
