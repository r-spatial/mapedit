#ifndef R_GEOMETRIES_H
#define R_GEOMETRIES_H

#include "geometries/utils/split/split.hpp"
#include "geometries/matrix/mat_to_df.hpp"
#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/lists/as_list.hpp"
#include "geometries/utils/attributes/attributes.hpp"
#include "geometries/utils/null/null.hpp"

namespace geometries {


  // adds the attribute to each geometry in a list
  // so is really used for collections
  inline SEXP make_geometries(
    Rcpp::List& l,
    Rcpp::List attributes,
    int& n_empty
  ) {
    // each 'row' is a geometry (i.e., a vector / POINT )
    bool has_class = attributes.length() > 0;
    R_xlen_t i;

    Rcpp::NumericMatrix geometry_mat = geometries::matrix::to_geometry_matrix( l );
    R_xlen_t n_rows = geometry_mat.nrow();
    Rcpp::List res( n_rows );

    for( i = 0; i < n_rows; ++i ) {
      Rcpp::NumericVector nv = geometry_mat( i, Rcpp::_ );
      if( geometries::utils::is_null_geometry( nv ) ) {
        n_empty++;
      }
      if( has_class ) {
        geometries::utils::attach_attributes( nv, attributes );
      }
      res[i] = nv;
    }
    return res;
  }

  // works on the assumption that every different outer-id is a different geometry
  // so any attributes are applied at the outer-id level
  inline SEXP make_geometries(
      Rcpp::List& l,
      Rcpp::IntegerVector& ids,
      Rcpp::IntegerVector& geometry_cols,
      Rcpp::List attributes,
      bool close = false,
      bool closed_attribute = false
  ) {

    bool has_class = attributes.length() > 0;

    R_xlen_t i, j;
    R_xlen_t n_id_cols = Rf_length( ids );

    Rcpp::List rleid( n_id_cols );

    for( i = n_id_cols - 1; i >= 0; --i ) {

      Rcpp::Range rng(0, i);
      Rcpp::IntegerVector rle_ids = ids[ rng ];

      bool last = i == n_id_cols - 1;
      bool first = i == 0;

      // Iff there's only one ID coulmn, the attributes need to be assigned inside 'split'
      // because once back here, a single-id column split doesn't get packaged up
      // AND iff nesting == n_id_cols (1)

      rleid( i ) = geometries::utils::split_by_id( l, rle_ids, geometry_cols, last, attributes, close, closed_attribute );

      // here the rleid(i) tells us which elements of rleid(i+1)
      // belong in which "package"
      Rcpp::List curr = rleid( i );

      Rcpp::IntegerVector nelems = curr["nelems"];
      Rcpp::IntegerVector curr_sums = curr["sums"];

      if( !last ) {
        Rcpp::List prev = rleid( i + 1 );
        Rcpp::IntegerVector prev_sums = prev["sums"];
        Rcpp::List prev_res = prev["res"];

        Rcpp::List curr_res( nelems.size() );
        R_xlen_t prev_idx = 0;

        for( j = 0; j < nelems.size(); ++j ) {

          R_xlen_t curr_sum = curr_sums[ j ];
          R_xlen_t start_prev_idx = prev_idx;
          R_xlen_t prev_sum = prev_sums[ prev_idx ];

          while( prev_sum != curr_sum ) { // prev_sum will always start <= to curr_sum
            prev_idx++;
            prev_sum = prev_sums[ prev_idx ];
          }
          prev_idx++;

          Rcpp::Range inner_rng( start_prev_idx, prev_idx - 1);
          Rcpp::List obj = prev_res[ inner_rng ];

          if( first && has_class && n_id_cols != 1 ) {
            geometries::utils::attach_attributes( obj, attributes );
          }
          curr_res( j ) = obj;
        }

        rleid( i ) = Rcpp::List::create(
          Rcpp::_["nelems"] = nelems,
          Rcpp::_["sums"] = curr_sums,
          Rcpp::_["res"] = curr_res
        );
      } else {
        // need to fill with the coordinates inside ["coords"]
        // it gets here first, because it works from the inside out
        SEXP coords = curr["coords"];

        if( first && has_class && n_id_cols != 1  ) {
           geometries::utils::attach_attributes( coords, attributes );
        }

        rleid( i ) = Rcpp::List::create(
          Rcpp::_["nelems"] = nelems,
          Rcpp::_["sums"] = curr_sums,
          Rcpp::_["res"] = coords
        );
      }
    } // for i

    Rcpp::List out = rleid[0];
    return out["res"];
  }

  inline SEXP make_geometries(
    SEXP& x,
    SEXP& ids,
    SEXP& geometry_cols,
    Rcpp::List attributes,
    bool close = false,
    bool closed_attribute = false
  ) {

    if( TYPEOF( ids ) != TYPEOF( geometry_cols ) ) {
      Rcpp::stop("geometries - id_columns and geometry_columns must be the same type");
    }

    Rcpp::IntegerVector int_ids = geometries::utils::sexp_col_int( x, ids );
    Rcpp::IntegerVector int_geom = geometries::utils::sexp_col_int( x, geometry_cols );

    Rcpp::List lst = geometries::utils::as_list( x );
    return make_geometries( lst, int_ids, int_geom, attributes, close, closed_attribute );
  }

  inline SEXP make_geometries(
    SEXP& x,
    SEXP& ids,
    SEXP& geometry_cols,
    bool close = false,
    bool closed_attribute = false
  ) {
    Rcpp::List attr;
    return make_geometries(x, ids, geometry_cols, attr, close, closed_attribute );
  }

} // geometries

#endif
