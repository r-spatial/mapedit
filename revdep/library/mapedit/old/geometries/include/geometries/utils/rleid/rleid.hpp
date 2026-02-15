#ifndef R_GEOMETRIES_UTILS_RLEID_H
#define R_GEOMETRIES_UTILS_RLEID_H

namespace geometries {
namespace utils {

  // from data.table
  // https://github.com/Rdatatable/data.table/blob/8b93bb22715b45d38acf185f40d573bda8748cb4/src/uniqlist.c#L164
  inline Rcpp::IntegerVector rleid( Rcpp::DataFrame& df, Rcpp::IntegerVector& ids ) {
    R_xlen_t i;
    R_xlen_t n_rows = df.nrow();
    //R_xlen_t n_cols = Rf_length( l );
    R_xlen_t n_id_cols = Rf_length( ids );

    Rcpp::IntegerVector ians( n_rows );
    int grp = 1;
    ians[0] = grp;

    for( i = 1; i < n_rows; ++i ) {
      bool same = true;
      int j = n_id_cols;
      while( --j >= 0 && same ) {
        SEXP jcol = VECTOR_ELT( df, ids[ j ] );
        switch( TYPEOF( jcol ) ) {
        case LGLSXP: {}
        case INTSXP: {
          same = INTEGER( jcol )[ i ] == INTEGER( jcol )[ i - 1 ];
          break;
        }
        case REALSXP: {
          long long *ll = (long long *)REAL( jcol );
          same = ll[ i ] == ll[ i - 1 ];
          break;
        }
        case STRSXP: {
          same = STRING_ELT( jcol, i ) == STRING_ELT( jcol, i - 1 );
          break;
        }
        default: {
          Rcpp::stop("geometries - unsupported id column type");
        }
        }
      }  // while end

      // at this point we now have a run-length-encoded vector
      ians[ i ] = ( grp += !same );
    }
    return ians;
  }

  // template < int RTYPE >
  // inline Rcpp::IntegerVector rleid_indices( Rcpp::Vector< RTYPE >& x ) {
  //
  // }

  inline Rcpp::IntegerVector rleid_indices( SEXP& x ) {

    //typedef typename Rcpp::traits::storage_type< RTYPE >::type T;
    R_xlen_t i;
    R_xlen_t len = Rf_length( x );
    R_xlen_t counter = 0;

    Rcpp::IntegerVector ians( len );
    //int grp = 1;
    ians[ 0 ] = 0;
    counter++;
    switch( TYPEOF( x ) ) {
      case INTSXP: case LGLSXP: {
        int *icol = INTEGER( x );
        for( i = 1; i < len; ++i ) {
          if( icol[i] != icol[i-1] ) {
            ians[ counter ] = i;
            counter++;
          }
        }
      } break;
      case REALSXP: {
        long long *lljcol = (long long *)REAL(x);
        for( i = 1; i < len; ++i ) {
         if( lljcol[i] != lljcol[i-1] ) {
            ians[ counter ] = i;
            counter++;
          }
        }
      } break;
      case STRSXP: {
        const SEXP *jd = STRING_PTR_RO( x );
        for (i = 1; i < len; ++i ) {
          if( jd[i] != jd[i-1] ) {
            ians[ counter ] = i;
            counter++;
          }
        }
      } break;
      default: {
        Rcpp::stop("geometries - unsupported vector type");
      }
    }
    Rcpp::Range rng(0, counter - 1);
    return ians[ rng ];
  }

  // inline Rcpp::IntegerVector rleid_indices( SEXP& x ) {
  //
  // }

  inline Rcpp::IntegerVector rleid_indices( Rcpp::List& l, Rcpp::IntegerVector& col ) {
    SEXP jcol = VECTOR_ELT( l, col[0] );
    return rleid_indices( jcol );
  }

} // utils
} // geometries


#endif
