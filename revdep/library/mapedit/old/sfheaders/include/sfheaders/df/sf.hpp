#ifndef R_SFHEADERS_DF_SF_H
#define R_SFHEADERS_DF_SF_H

#include "sfheaders/df/sfc.hpp"
#include "geometries/utils/vectors/vectors.hpp"
#include "geometries/utils/lists/list.hpp"
#include "geometries/coordinates/dimensions.hpp"

#include <Rcpp.h>

namespace sfheaders {
namespace df {

  inline Rcpp::List sf_to_df(
      Rcpp::DataFrame& sf,
      Rcpp::List& sfc,
      std::string& geom_column,
      Rcpp::IntegerMatrix& sfc_coordinates,
      bool fill = false
  ) {

    R_xlen_t n_geometries = sfc_coordinates.nrow();
    R_xlen_t total_coordinates = sfc_coordinates( n_geometries - 1 , 1 );
    total_coordinates = total_coordinates + 1;

    Rcpp::List sfc_df = sfheaders::df::get_sfc_coordinates( sfc, total_coordinates );

    if( !fill ) {
      return sfc_df;  // #nocov
    }

    R_xlen_t sfc_cols = sfc_df.length();

    Rcpp::IntegerVector expanded_index( total_coordinates );

    R_xlen_t i;
    R_xlen_t j;
    R_xlen_t counter = 0;

    for( i = 0; i < n_geometries; ++i ) {
      R_xlen_t expand_by = sfc_coordinates( i, 1 ) - sfc_coordinates( i, 0 ) + 1;

      for( j = 0; j < expand_by; ++j ) {
        expanded_index[ counter + j ] = i;
      }
      counter = counter + expand_by;
    }

    R_xlen_t n_col = sf.ncol();
    Rcpp::List res( n_col - 1 + sfc_cols ); // -1 to remove the geometry

    Rcpp::CharacterVector res_names( n_col - 1 + sfc_cols );

    // the 'non-geometry' names of the data.frame
    Rcpp::CharacterVector sf_names = sf.names();
    // iff these names are in res_names

    R_xlen_t name_position = 0;
    for( i = 0; i < n_col; ++i ) {

      if( sf_names[ i ] != geom_column ) {
        res_names[ name_position ] = sf_names[ i ];
        SEXP v = sf[ i ];
        geometries::utils::expand_vector( res, v, expanded_index, name_position );
        name_position += 1;
      }
    }

    Rcpp::CharacterVector sfc_df_names = sfc_df.names();

    // in sfc.hpp I define geometry columns with names 'x','y','z','m'
    // so I know these will be geometry columns
    Rcpp::StringVector geometry_columns = {"x","y","z","m"};
    Rcpp::LogicalVector keep_columns( sfc_df_names.length() );
    // set true / false if the sfc_df_names are in sfc.hpp names
    int is_in;

    for( i = 0; i < sfc_df_names.length(); ++i ) {
      Rcpp::String geom = sfc_df_names[ i ];
      is_in = geometries::utils::where_is( geom, geometry_columns );
      keep_columns[ i ] = is_in == -1 ? false : true;
    }


    for( i = 0; i < sfc_cols; ++i ) {
      Rcpp::String this_name = sfheaders::utils::unique_name( sfc_df_names[ i ], res_names );
      // unique-ify 'this_name;
      sfc_df_names[ i ] = this_name; // use these names as sfc_columns attr

      res_names[ i + n_col - 1 ] = this_name;
      res[ i + n_col - 1 ] = sfc_df[ i ];
    }

    res.attr("sfc_columns") = sfc_df_names[ keep_columns ];
    return sfheaders::utils::make_dataframe( res, total_coordinates, res_names );

  }

  inline Rcpp::List sf_to_df(
    Rcpp::DataFrame& sf,
    bool fill = false
  ) {
    if( !sf.hasAttribute("sf_column") ) {
      Rcpp::stop("sfheaders - sf_column not found");
    }

    std::string geom_column = sf.attr("sf_column");
    Rcpp::List sfc = sf[ geom_column ];

    Rcpp::List dims = geometries::coordinates::geometry_dimensions( sfc );
    Rcpp::IntegerMatrix sfc_coordinates = dims["dimensions"];

    return sf_to_df( sf, sfc, geom_column, sfc_coordinates, fill );
  }

  inline Rcpp::List sf_to_df(
      Rcpp::DataFrame& sf,
      Rcpp::List& sfc,
      std::string& geom_column,
      Rcpp::IntegerMatrix& sfc_coordinates,
      Rcpp::StringVector& unlist,
      bool fill = false
  ) {
    if( !sf.hasAttribute("sf_column") ) {
      Rcpp::stop("sfheaders - sf_column not found");
    }

    if( Rf_isNull( unlist ) ) {
      return sf_to_df( sf, fill );
    }

    // issue 75 - ignore undefined 'unlist' columns
    Rcpp::StringVector sf_names = sf.names();
    Rcpp::IntegerVector unlist_idx = geometries::utils::where_is( unlist, sf_names );
    unlist = unlist[ unlist_idx >= 0 ];

    R_xlen_t n_unlist = unlist.size();
    R_xlen_t i;
    Rcpp::List to_unlist( n_unlist );

    for( i = 0; i < n_unlist; ++i ) {
      const char *s = unlist[ i ];
      Rcpp::List lst = sf[ s ];
      to_unlist[ i ] = geometries::utils::unlist_list( lst );
      // iff total_size == sf.nrow();
      // then it's not a list column.
    }

    to_unlist.names() = unlist;

    Rcpp::DataFrame res = sf_to_df( sf, sfc, geom_column, sfc_coordinates, fill );

    R_xlen_t n_row = res.nrow();

    for( i = 0; i < n_unlist; ++i ) {
      const char *s = unlist[ i ];
      SEXP unlisted_col = to_unlist[ i ];
      R_xlen_t n = geometries::utils::sexp_length( unlisted_col );
      if( n == sf.nrow() ) {
        continue; // issue 76
      }
      if( n != n_row ) {
        Rcpp::stop("sfheaders - unlisted column doesn't have the correct number of rows");
      }
      res[ s ] = to_unlist[ i ];
    }

    return sfheaders::utils::make_dataframe( res, n_row );
  }

  inline Rcpp::List sf_to_df(
      Rcpp::DataFrame& sf,
      Rcpp::StringVector& unlist,
      bool fill = false
  ) {
    std::string geom_column = sf.attr("sf_column");
    Rcpp::List sfc = sf[ geom_column ];

    Rcpp::List dims = geometries::coordinates::geometry_dimensions( sfc );
    Rcpp::IntegerMatrix sfc_coordinates = dims["dimensions"];

    return sf_to_df( sf, sfc, geom_column, sfc_coordinates, unlist, fill );
  }


} // df
} // sfheaders

#endif
