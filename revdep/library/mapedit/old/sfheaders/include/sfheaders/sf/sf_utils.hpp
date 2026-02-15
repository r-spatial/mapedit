#ifndef R_SFHEADERS_SF_UTILS_H
#define R_SFHEADERS_SF_UTILS_H

#include "geometries/utils/vectors/vectors.hpp"
#include "geometries/utils/lists/list.hpp"
#include "geometries/nest/nest.hpp"
#include "geometries/coordinates/dimensions.hpp"

namespace sfheaders {
namespace sf {

  inline void id_length_check(
    SEXP& ids,
    Rcpp::List& sfc
  ) {
    if( Rf_length( ids ) != sfc.length() ) {
      Rcpp::stop("sfheaders - error indexing lines, perhaps caused by un-ordered data? ");
    }
  }

  // issue 41 - will subset a vector
  inline SEXP subset_properties(
      SEXP& v,
      Rcpp::IntegerVector& subset_index
  ) {

    switch( TYPEOF( v ) ) {
    case LGLSXP: {
      Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( v );
      return lv[ subset_index ];
    }
    case INTSXP: {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( v );
      return iv[ subset_index ];
    }
    case REALSXP: {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( v );
      return nv[ subset_index ];
    }
    case STRSXP: {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( v );
      return sv[ subset_index ];
    }
    case CPLXSXP: {
      Rcpp::ComplexVector cv = Rcpp::as< Rcpp::ComplexVector >( v );
      return cv[ subset_index ];
    }
    case RAWSXP: {
      Rcpp::RawVector rv = Rcpp::as< Rcpp::RawVector >( v );
      return rv[ subset_index ];
    }
    default: {
      Rcpp::stop("sfheaders - unsupported column type using keep = TRUE"); // #nocov
    }
    }
  }

  inline void attach_dataframe_attributes(
      Rcpp::List& df,
      R_xlen_t& n_row,
      std::string geometry_column = "geometry"
  ) {
    df.attr("class") = Rcpp::CharacterVector::create("sf", "data.frame");
    df.attr("sf_column") = geometry_column;

    if( n_row == 0 ) {
      df.attr("row.names") = Rcpp::IntegerVector(0);  // #nocov
    } else {
      Rcpp::IntegerVector rn = Rcpp::seq( 1, n_row );
      df.attr("row.names") = rn;
    }
  }

  inline Rcpp::StringVector make_names( R_xlen_t& n_col ) {
    R_xlen_t i;
    Rcpp::StringVector res( n_col );
    for( i = 0; i < n_col; ++i ) {
      std::ostringstream os;
      os << "V" << ( i + 1 );
      res[ i ] = os.str();
    }
    return res;
  }

  // inline void unlist_indexes(
  //   const Rcpp::List& lst_indexes,
  //   Rcpp::IntegerVector& indexes,
  //   R_xlen_t& start_idx
  // ) {
  //   R_xlen_t n_geometries = lst_indexes.size();
  //   R_xlen_t i;
  //   for( i = 0; i < n_geometries; ++i ) {
  //     switch( TYPEOF( lst_indexes[ i ] ) ) {
  //       case VECSXP: {
  //         unlist_indexes( lst_indexes[ i ], indexes, start_idx );
  //         break;
  //       }
  //       case INTSXP: {
  //         Rcpp::IntegerVector elements = Rcpp::as< Rcpp::IntegerVector >( lst_indexes [ i ] );
  //         R_xlen_t end_position = start_idx + elements.length() - 1;
  //         Rcpp::IntegerVector idx = Rcpp::seq( start_idx, end_position );
  //         indexes[ idx ] = elements;
  //         start_idx = end_position + 1;
  //         break;
  //       }
  //       default: {
  //         Rcpp::stop("sfheaders - error trying to unlist indexes");
  //       }
  //     }
  //   }
  // }

  //' @param start_idx is the row of the input data.frame corresponding to 'this'
  //' matrix
  //' @param total_length to keep track of the total length of the new property index
  //' column we will be creating
  inline SEXP property_indexes(
    SEXP& obj,
    R_xlen_t& start_idx,
    R_xlen_t& total_length
  ) {

    switch( TYPEOF( obj ) ) {
    case VECSXP: {

      Rcpp::List lst = Rcpp::as< Rcpp::List >( obj );
      Rcpp::List res( lst.size() );
      R_xlen_t i;

      for( i = 0; i < lst.size(); ++i ) {
        SEXP inner_obj = lst[ i ];
        res[ i ] = property_indexes( inner_obj, start_idx, total_length );
      }
      return res;
    }
    default: {
      if( !Rf_isMatrix( obj ) ) {
        Rcpp::stop("geometries - error filling list column. Expecting either matrix or list");
      }

      R_xlen_t has_been_closed = geometries::utils::has_been_closed_attribute( obj );
      R_xlen_t n_row = geometries::utils::sexp_n_row( obj );

      R_xlen_t input_n_row = n_row - has_been_closed; // iff the matrix was closed, a row was appended to the original
      R_xlen_t res_size = input_n_row + has_been_closed;

      Rcpp::IntegerVector idx( res_size );
      total_length = total_length + res_size;

      R_xlen_t k;
      for( k = 0; k < input_n_row; ++k ) {
        idx[ k ] = k + start_idx;
      }
      if( has_been_closed == 1 ) {
        idx[ res_size - 1 ] = start_idx;
      }

      // update the start_idx
      start_idx = start_idx + input_n_row;
      return idx;
    }
    }

    return Rcpp::List();
  }

  //' @param lst_indexes the list of property indexes. This list will be the result of
  //' property_indexes(), where each list element corresponds to a matrix (geometry)
  //'
  inline Rcpp::List fill_list(
    const Rcpp::List& lst_indexes,
    SEXP& v
  ) {

    R_xlen_t n = lst_indexes.size();
    R_xlen_t i;
    Rcpp::List res( n );

    for( i = 0; i < n; ++i ) {
      switch( TYPEOF( lst_indexes[ i ] ) ) {
        case VECSXP: {
          // if it's a list, go back around
          res[ i ] = fill_list( lst_indexes[ i ], v );
          break;
        }
        case INTSXP: {
          // the elements of the lst_indexes tell us which values of 'v' we need
          Rcpp::IntegerVector idx = Rcpp::as< Rcpp::IntegerVector >( lst_indexes[ i ] );
          res[ i ] = subset_properties( v, idx );
          break;
        }
        default: {
          Rcpp::stop("sfheaders - filling lists requires integer indexes");
        }
      }
    }
    return res;
  }

  // creates a list-column the same dimension as the `sfc` object
  // i.e, a polygon will be a list-of-lists,
  // where each list element will (should) have the same number of rows
  // as the matrix (geometry)
  // because it assumes each coordinate has an associated data elment inside the list
  // for polygons that have been closed, it adds the first element to the end of the
  // vector, to account for the repeated coordinate
  inline Rcpp::List create_property_indexes(
    Rcpp::List& sfc
  ) {

    R_xlen_t i;
    R_xlen_t n_geometries = sfc.size();
    Rcpp::List lst_indexes( n_geometries );

    R_xlen_t start_idx = 0;
    R_xlen_t total_length = 0;
    for( i = 0; i < n_geometries; ++i ) {
      SEXP sfg = sfc[ i ];
      lst_indexes[ i ] = property_indexes( sfg, start_idx, total_length );
    }

    return lst_indexes;
  }

  inline Rcpp::List create_sf(
      Rcpp::List& data,
      Rcpp::List& sfc,
      Rcpp::IntegerVector& id_column,
      Rcpp::IntegerVector& property_cols,
      Rcpp::IntegerVector& list_column_idx,
      Rcpp::IntegerVector& geometry_idx,
      bool closed_attributes = false
  ) {

    bool has_id = !Rf_isNull( id_column ) && Rf_length( id_column ) > 0;
    bool has_properties = !Rf_isNull( property_cols ) && Rf_length( property_cols ) > 0;
    bool has_list_cols = !Rf_isNull( list_column_idx );

    Rcpp::List lst_indexes; // if there are list columns, this wills tore the row-indexes of the properties
    // used to fill the list columns

    R_xlen_t data_n_cols = Rf_length( data );

    R_xlen_t n_properties = property_cols.length();
    //_xlen_t n_col = has_id ? property_cols.length() : property_cols.length() + 1;
    Rcpp::StringVector data_names( data_n_cols );

    R_xlen_t total_cols = n_properties + 1 + has_id; // +1 == sfc
    Rcpp::List sf( total_cols );
    Rcpp::StringVector res_names( total_cols );
    R_xlen_t i;

    if( !Rf_isNull( data.names() ) ) {
      data_names = data.names();
    } else {

      // the property_cols object points to index of data_names
      // so we need the maximum property_cols value

      data_names = make_names( data_n_cols );
      // TODO/ this needs to be max( property_cols );
      // or at leaste max( n_properties ) of the input data object
    }

    if( has_id ) {
      int id = id_column[0];
      SEXP ids = VECTOR_ELT( data, id );
      SEXP unique_ids = geometries::utils::get_sexp_unique( ids );
      id_length_check( unique_ids, sfc );

      sf[ 0 ] = unique_ids;
      res_names[ 0 ] = data_names[ id ];
    }

    if( has_properties ) {

      if( list_column_idx.size() > 0 ) {
        // only need to do this once, so it's outside the main loop.
        lst_indexes = create_property_indexes( sfc );
      }

      for( i = 0; i < n_properties; ++i ) {

        int idx = property_cols[ i ];
        bool is_list_column = has_list_cols && ( std::find( list_column_idx.begin(), list_column_idx.end(), idx ) != list_column_idx.end()  );
        SEXP v = data[ idx ];

        if( is_list_column ) {
          sf[ i + has_id ] = fill_list( lst_indexes, v );
        } else {
          sf[ i + has_id ] = subset_properties( v, geometry_idx );
        }
        res_names[ i + has_id ] = data_names[ idx ];
      }
    }

    // TODO
    if( closed_attributes ) {
      // remove these from each matrix because
      // we don't want them hanging around
    }

    // make data.frame
    Rcpp::String sfc_name = "geometry";

    sf[ total_cols - 1 ] = sfc;
    res_names[ total_cols - 1 ] = sfc_name;
    sf.names() = res_names;

    R_xlen_t n_row = geometry_idx.length();

    sfheaders::sf::attach_dataframe_attributes( sf, n_row );

    return sf;
  }


  inline SEXP make_sf( Rcpp::List& sfc ) {

    Rcpp::List df = Rcpp::List::create(
      Rcpp::Named("geometry") = sfc
    );

    R_xlen_t n_row = sfc.length();
    attach_dataframe_attributes( df, n_row );

    return df;

  }

  inline SEXP make_sf( Rcpp::List& sfc, Rcpp::IntegerVector& iv ) {

    Rcpp::List df = Rcpp::List::create(
      Rcpp::Named("id") = iv,
      Rcpp::Named("geometry") = sfc
    );

    R_xlen_t n_row = sfc.length();
    attach_dataframe_attributes( df, n_row );

    return df;
  }

  inline SEXP make_sf( Rcpp::List& sfc, Rcpp::NumericVector& nv ) {
    Rcpp::List df = Rcpp::List::create(
      Rcpp::Named("id") = nv,
      Rcpp::Named("geometry") = sfc
    );

    R_xlen_t n_row = sfc.length();
    attach_dataframe_attributes( df, n_row );

    return df;
  }

  inline SEXP make_sf( Rcpp::List& sfc, Rcpp::StringVector& sv ) {
    Rcpp::List df = Rcpp::List::create(
      Rcpp::Named("id") = sv,
      Rcpp::Named("geometry") = sfc
    );

    R_xlen_t n_row = sfc.length();
    attach_dataframe_attributes( df, n_row );

    return df;
  }


  inline SEXP make_sf(
      Rcpp::List& sfc,
      SEXP& ids
  ) {

    if( Rf_isNull( ids ) ) {
      make_sf( sfc );          // #nocov
    }

    switch( TYPEOF( ids ) ) {
      case LGLSXP: {}
      case INTSXP: {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( ids );
        return make_sf( sfc, iv );
      }
      case REALSXP: {
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( ids );
        return make_sf( sfc, nv );
      }
      case STRSXP: {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( ids );
        return make_sf( sfc, sv );
      }
      default: {
        Rcpp::stop("sfheaders - invalid sf id columns");   // #nocov
      }
    }

    return Rcpp::List::create();
  }


} // sf
} // sfheaders

#endif
