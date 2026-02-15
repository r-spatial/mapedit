#ifndef R_SFHEADERS_CAST_SF_H
#define R_SFHEADERS_CAST_SF_H

#include "sfheaders/sf/sf_utils.hpp"
#include "sfheaders/cast/sfc_cast.hpp"

#include "geometries/utils/vectors/vectors.hpp"

#include <Rcpp.h>

namespace sfheaders {
namespace cast {

  inline Rcpp::List cast_sf(
      Rcpp::DataFrame& sf,
      std::string& cast_to,
      SEXP list_columns,
      bool close = true
  ) {
    if( !sf.hasAttribute("sf_column") ) {
      Rcpp::stop("sfheaders - sf_column not found");
    }

    Rcpp::IntegerVector iv_list_columns;
    Rcpp::IntegerVector iv_geom_column;

    if( !Rf_isNull( list_columns ) ) {
      // need to use 'obj' here because 'list_columns' may be a string, but 'data'
      // has been made into an unnamed list
      iv_list_columns = geometries::utils::sexp_col_int( sf, list_columns );
    }

    Rcpp::StringVector df_names = sf.names();
    R_xlen_t n_names = df_names.length();
    R_xlen_t i, j;
    R_xlen_t n_row = sf.nrow();
    R_xlen_t n_col = sf.ncol();

    std::string geom_column = sf.attr("sf_column");
    iv_geom_column = geometries::utils::sexp_col_int( df_names, geom_column );

    Rcpp::List sfc = sf[ geom_column ];
    Rcpp::IntegerVector n_results = count_new_sfc_objects( sfc, cast_to );

    R_xlen_t total_coordinates = Rcpp::sum( n_results );

    Rcpp::IntegerVector expanded_index( total_coordinates );

    // other than the sfc column, expand all the other (non-list) columns by 'n_results'
    R_xlen_t counter = 0;
    for( i = 0; i < n_row; ++i ) {
      R_xlen_t expand_by = n_results[ i ];

      for( j = 0; j < expand_by; ++j ) {
        expanded_index[ counter + j ] = i;
      }
      counter = counter + expand_by;
    }

    Rcpp::List casted_sfc = sfheaders::cast::cast_sfc( sfc, n_results, cast_to, close );

    Rcpp::List sf_res( n_names );
    // loop over each of the df_names which aren't the geometry or list-columns
    // then add on the created_sfc;
    Rcpp::StringVector res_names( n_col );
    R_xlen_t column_counter = 0;


    for( i = 0; i < n_names; ++i ) {
      // iff this_name != geom_column, expand the vector and doene.
      Rcpp::String this_name = df_names[ i ];
      std::string str_name = this_name;

      if( str_name != geom_column ) {

        int is_in = geometries::utils::where_is( i, iv_list_columns );

        if( is_in >= 0 ) {

          Rcpp::List v = sf[ i ];

          sf_res[ column_counter ] = sfheaders::cast::cast_list( v, sfc, n_results, cast_to );
        } else {

          SEXP v = sf[ i ];
          geometries::utils::expand_vector( sf_res, v, expanded_index, column_counter );
        }

        res_names[ column_counter ] = str_name;
        ++column_counter;
      }
    }

    // append 'geom_column' to res_columns;
    res_names[ column_counter ] = geom_column;

    sf_res[ column_counter ] = casted_sfc;
    sf_res.names() = res_names;
    sfheaders::sf::attach_dataframe_attributes( sf_res, total_coordinates, geom_column );
    return sf_res;

  }

} // cast
} // sfheaders

#endif
