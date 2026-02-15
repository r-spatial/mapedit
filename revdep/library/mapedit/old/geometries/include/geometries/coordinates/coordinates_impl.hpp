#ifndef R_GEOMETRIES_GEOMETRIES_COORDINATES_IMPL_H
#define R_GEOMETRIES_GEOMETRIES_COORDINATES_IMPL_H

#include "geometries/coordinates/coordinates.hpp"
#include "geometries/utils/dataframe/dataframe.hpp"

namespace geometries {
namespace coordinates {

  inline Rcpp::CharacterVector coordinate_column_names(
    R_xlen_t& nest,
    R_xlen_t& dim
  ) {
    Rcpp::CharacterVector column_names( nest + dim + 1 );
    column_names[0] = "id";

    R_xlen_t i;

    for( i = 0; i < nest; ++i ) {
      std::ostringstream os;
      os << "id" << ( i + 1 );
      column_names[ i + 1 ] = os.str();
    }

    for( i = 0; i < dim; ++i ) {
      std::ostringstream os;
      os << "c" << ( i + 1 );
      column_names[ i + nest + 1 ] = os.str();
    }
    return column_names;
  }

inline Rcpp::DataFrame coordinates_impl(
      Rcpp::Vector< REALSXP >& geometry
  ) {
    R_xlen_t geometry_length = geometry.length();
    Rcpp::List res = geometries::coordinates::coordinates( geometry, geometry_length );
    R_xlen_t rows = 1;
    R_xlen_t nest = 0;

    // re-defining the 'dim', because 'geometry_length' gets updated inside coordinats() (although I'm not sure where or why...)
    R_xlen_t dim = geometry.length();
    Rcpp::CharacterVector column_names = coordinate_column_names( nest, dim );
    return geometries::utils::make_dataframe( res, rows, column_names );
  }

  inline Rcpp::DataFrame coordinates_impl(
      Rcpp::Matrix< REALSXP >& geometry
  ) {
    R_xlen_t geometry_rows = geometry.nrow();
    Rcpp::List res = geometries::coordinates::coordinates( geometry, geometry_rows );
    R_xlen_t nest = 0;
    R_xlen_t dim = geometry.ncol();
    Rcpp::CharacterVector column_names = coordinate_column_names( nest, dim );
    return geometries::utils::make_dataframe( res, geometry_rows, column_names );
  }

  inline Rcpp::DataFrame coordinates_impl(
      Rcpp::List& geometries
  ) {
    Rcpp::List dimensions = geometries::coordinates::geometry_dimensions( geometries );

    Rcpp::IntegerMatrix dim = dimensions["dimensions"];
    R_xlen_t max_nest = dimensions["max_nest"];
    R_xlen_t max_dimension = dimensions["max_dimension"];

    R_xlen_t n_geometries = dim.nrow();
    R_xlen_t total_coordinates = dim( n_geometries - 1, 1 );
    total_coordinates = total_coordinates + 1;

    // the coordinate column start index in the result list is the same
    // for every geometry, regardless of dimension
    // i.e., so all the "X" coords line up, and all the "Y"s, then any extra follow from there
    R_xlen_t coord_col_idx = max_nest + 1;

    // set up result list
    R_xlen_t n_cols = max_nest + max_dimension + 1;
    Rcpp::List res( n_cols );
    R_xlen_t i, j;


    for( i = 0; i < n_cols; ++i ) {
      Rcpp::Vector< REALSXP > nv( total_coordinates, Rcpp::Vector< REALSXP >::get_na() );
      res[ i ] = nv;
    }

    for( i = 0; i < n_geometries; ++i ) {

      SEXP geometry = geometries[ i ];
      Rcpp::IntegerVector dimension = dim( i, Rcpp::_ );
      // 0 = start index
      // 1 = end index
      // 2 = dimension
      // 3 = nest
      // 4 = RTYPE
      R_xlen_t start_row_idx = dimension[0];

      double geometry_id = 1;
      geometries::coordinates::coordinates( geometry, res, start_row_idx, coord_col_idx, geometry_id );
    } // ++i

    // for the id vector, the dimensions matrix tells me the start and end of each geometry
    // so can I make a vector this length, and fill it
    Rcpp::Vector< REALSXP > shape_id = Rcpp::no_init( total_coordinates );

    for( i = 0; i < n_geometries; ++i ) {
      R_xlen_t start = dim( i, 0 );
      R_xlen_t end = dim( i, 1 );
      for( j = start; j <= end; ++j ) {
        shape_id[ j ] = i + 1; // so id's start at 1
      }
    }

    res[0] = shape_id;
    Rcpp::CharacterVector column_names = coordinate_column_names( max_nest, max_dimension );
    return geometries::utils::make_dataframe( res, total_coordinates, column_names  );
  }

inline SEXP coordinates_impl(
      SEXP& geometries
  ) {

    switch( TYPEOF( geometries ) ) {
      case INTSXP: {}
      case REALSXP: {
        if( Rf_isMatrix( geometries ) ) {
          Rcpp::Matrix< REALSXP > mat = Rcpp::as< Rcpp::Matrix< REALSXP > >( geometries );
          return coordinates_impl( mat );
        } else { // vector
          Rcpp::Vector< REALSXP > vec = Rcpp::as< Rcpp::Vector< REALSXP > >( geometries );
          return coordinates_impl( vec );
        }
      }
      case VECSXP: {
        Rcpp::List lst = Rcpp::as< Rcpp::List >( geometries );
        return coordinates_impl( lst );
      }
      default: {
        Rcpp::stop("geometries - only vectors, matrices and lists are supported");
      }
    }
    return Rcpp::List::create(); // #nocov never reaches
  }

} // coordinates
} // geometries

#endif
