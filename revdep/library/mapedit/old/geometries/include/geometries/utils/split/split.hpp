#ifndef R_GEOMETRIES_UTILS_SPLIT_H
#define R_GEOMETRIES_UTILS_SPLIT_H

#include "geometries/matrix/to_mat.hpp"
#include "geometries/utils/close/close.hpp"
#include "geometries/utils/attributes/attributes.hpp"

namespace geometries {
namespace utils {

  //' Split By Id
  //'
  //' @param last bool indicates if the ids contain the outer-most id value. For example,
  //' when making a polygon, last will indicate if the ids vector contains the polygon_id
  //' @param closed_attribute bool indicating if each matrix returned should
  //' have a 'has_been_closed' attribute attached
  inline SEXP split_by_id(
      Rcpp::List& l,
      Rcpp::IntegerVector& ids,
      Rcpp::IntegerVector& geometry_cols,
      bool last,
      Rcpp::List attributes,
      bool close = false,
      bool closed_attribute = false
  ) {

    bool has_class = attributes.length() > 0;

    // matrix of geometries
    Rcpp::NumericMatrix geometry_mat = geometries::matrix::to_geometry_matrix( l, geometry_cols );

    R_xlen_t i;
    R_xlen_t n_rows = Rf_length( VECTOR_ELT( l, 0 ) );
    R_xlen_t n_id_cols = Rf_length( ids );

    Rcpp::IntegerVector nelems( n_rows );
    Rcpp::IntegerVector sums( n_rows );

    // values for keeping track of the indexes of 'l' (rows) which form the individual geometries;
    int start = 0;
    int end = 0;
    Rcpp::List res( n_rows ); // arbitrarily start it at the max possible?
    R_xlen_t res_counter = 0;
    R_xlen_t n_elements = 1;
    R_xlen_t cumulative_size = 0;

    for( i = 1; i < n_rows; ++i ) {
      bool same = true;
      int j = n_id_cols;

      // this while loop is taken from geometries::utils::rleid()
      while( --j >= 0 && same ) {
        SEXP jcol = VECTOR_ELT( l, ids[ j ] );
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
      n_elements += same;

      // at the point where same == 0 we have a new rleid
      if( !same ) {

        nelems[ res_counter ] = n_elements;
        cumulative_size = n_elements + cumulative_size;
        sums[ res_counter ] = cumulative_size;
        n_elements = 1;

        if( last ) {
          end = i - 1;

          Rcpp::Range row_rng( start, end );
          Rcpp::NumericMatrix res_mat = geometry_mat( row_rng, Rcpp::_ );
          if( close ) {
            bool is_closed = geometries::utils::matrix_is_closed( res_mat );
            res_mat = geometries::utils::close_matrix( res_mat );

            // it it started as 'not closed' , and it's here, then it has to have
            // been closed
            if( closed_attribute && !is_closed) {
              Rcpp::List att = Rcpp::List::create(
                Rcpp::_["closed"] = "has_been_closed"
              );
              geometries::utils::attach_attributes( res_mat, att );
            }
          }

          if( has_class && n_id_cols == 1 ) {
            geometries::utils::attach_attributes( res_mat, attributes );
          }

          res[ res_counter ] = res_mat;

          start = i;
        }
        res_counter++;
      }
    }

    nelems[ res_counter ] = n_elements;

    cumulative_size = n_elements + cumulative_size;
    sums[ res_counter ] = cumulative_size;

    Rcpp::Range rng( 0, res_counter );

    if( last ) {
      end = i - 1;

      Rcpp::Range row_rng( start, end );
      Rcpp::NumericMatrix res_mat = geometry_mat( row_rng, Rcpp::_ );

      if( close ) {
        bool is_closed = geometries::utils::matrix_is_closed( res_mat );
        res_mat = geometries::utils::close_matrix( res_mat );

        if( closed_attribute && !is_closed) {
          Rcpp::List att = Rcpp::List::create(
            Rcpp::_["closed"] = "has_been_closed"
          );
          geometries::utils::attach_attributes( res_mat, att );
        }

      }

      if( has_class && n_id_cols == 1 ) {
        geometries::utils::attach_attributes( res_mat, attributes);
      }

      res[ res_counter ] = res_mat;

      // we can remove any res elements AFTER res_counter
      // because by this point we've filled everything
      Rcpp::Range coord_rng( 0, res_counter );

      return Rcpp::List::create(
        Rcpp::_["nelems"] = nelems[ rng ],
        Rcpp::_["sums"] = sums[ rng ],
        Rcpp::_["coords"] = res[ coord_rng ]
      );
    }

    return Rcpp::List::create(
      Rcpp::_["nelems"] = nelems[ rng ],
      Rcpp::_["sums"] = sums[ rng ]
    );

  }

} // utils
} // geoemtries


#endif
