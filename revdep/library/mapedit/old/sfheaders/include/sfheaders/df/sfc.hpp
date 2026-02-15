#ifndef R_SFHEADERS_DF_SFC_H
#define R_SFHEADERS_DF_SFC_H

#include "sfheaders/df/sfg.hpp"
#include "sfheaders/df/utils.hpp"

#include "geometries/coordinates/dimensions.hpp"
#include "geometries/utils/lists/collapse.hpp"

#include <Rcpp.h>

namespace sfheaders {
namespace df {

  inline void column_index_check( Rcpp::IntegerVector& sfg_cols, R_xlen_t& n_col ) {
    if( sfg_cols.length() != n_col ) {
      Rcpp::stop("sfheaders - column indexing error - please report this issue, along with an example, at github.com/dcooley/sfheaders");  // #nocov
    }
  }

  inline Rcpp::List setup_result( R_xlen_t& total_coordinates ) {

    Rcpp::NumericVector sfc_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector sfg_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector geometrycollection_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector multipolygon_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector polygon_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector multilinestring_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector linestring_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector multipoint_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector point_id_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector x_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector y_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector m_res( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector z_res( total_coordinates, Rcpp::NumericVector::get_na() );

    Rcpp::List res = Rcpp::List::create(
      Rcpp::_["sfc_id"] = sfc_id_res,
      Rcpp::_["sfg_id"] = sfg_id_res,
      Rcpp::_["geometrycollection_id"] = geometrycollection_id_res,
      Rcpp::_["multipolygon_id"] = multipolygon_id_res,
      Rcpp::_["polygon_id"] = polygon_id_res,
      Rcpp::_["multilinestring_id"] = multilinestring_id_res,
      Rcpp::_["linestring_id"] = linestring_id_res,
      Rcpp::_["multipoint_id"] = multipoint_id_res,
      Rcpp::_["point_id"] = point_id_res,
      Rcpp::_["x"] = x_res,
      Rcpp::_["y"] = y_res,
      Rcpp::_["z"] = z_res,
      Rcpp::_["m"] = m_res
    );
    return res;
  }

  inline void dim_error() { // #nocov
    Rcpp::stop("sfheaders - unknown geometry dimension");    // #nocov
  }

  inline Rcpp::IntegerVector get_sfg_cols( R_xlen_t& n_col, int geometry, std::string& dim ) {

    switch( geometry ) {
    case sfheaders::sfg::SFG_POINT: {}
    case sfheaders::sfg::SFG_MULTIPOINT: {}
    case sfheaders::sfg::SFG_LINESTRING: {
      if( dim == "XY" ) {
      return Rcpp::IntegerVector({ X_COLUMN, Y_COLUMN });
    } else if( dim == "XYZM" ) {
      return Rcpp::IntegerVector({ X_COLUMN, Y_COLUMN, Z_COLUMN, M_COLUMN });
    } else if ( dim == "XYZ" ) {
      return Rcpp::IntegerVector({ X_COLUMN, Y_COLUMN, Z_COLUMN });
    } else if ( dim == "XYM" ) {
      return Rcpp::IntegerVector({ X_COLUMN, Y_COLUMN, M_COLUMN });
    } else {
      dim_error();  // #nocov
    }
    }
    case sfheaders::sfg::SFG_MULTILINESTRING: {}
    case sfheaders::sfg::SFG_POLYGON: {
      if( dim == "XY" ) {
      return Rcpp::IntegerVector({ LINESTRING_COLUMN, X_COLUMN, Y_COLUMN });
    } else if( dim == "XYZM" ) {
      return Rcpp::IntegerVector({ LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, Z_COLUMN, M_COLUMN });
    } else if ( dim == "XYZ" ) {
      return Rcpp::IntegerVector({ LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, Z_COLUMN });
    } else if ( dim == "XYM" ) {
      return Rcpp::IntegerVector({ LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, M_COLUMN });
    } else {
      dim_error();  // #nocov
    }
    }
    case sfheaders::sfg::SFG_MULTIPOLYGON: {
      if( dim == "XY" ) {
      return Rcpp::IntegerVector({ POLYGON_COLUMN, LINESTRING_COLUMN, X_COLUMN, Y_COLUMN });
    } else if( dim == "XYZM" ) {
      return Rcpp::IntegerVector({ POLYGON_COLUMN, LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, Z_COLUMN, M_COLUMN });
    } else if ( dim == "XYZ" ) {
      return Rcpp::IntegerVector({ POLYGON_COLUMN, LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, Z_COLUMN });
    } else if ( dim == "XYM" ) {
      return Rcpp::IntegerVector({ POLYGON_COLUMN, LINESTRING_COLUMN, X_COLUMN, Y_COLUMN, M_COLUMN });
    } else {
      dim_error(); // #nocov
    }

    }
    default: {
      Rcpp::stop("sfheaders - unknown geometry type");  // #nocov
    }
    }

    return Rcpp::IntegerVector(); // #nocov never reached
  }

  // sfcs are a list of sfgs.
  // they can be mixed, or individual.
  // if indiidual, loop over each one and extract the sfgs, list by list, then collapse the lists??

  inline Rcpp::List get_sfg_coordinates( SEXP& sfg, R_xlen_t& sfc_rows, int SFG_TYPE ) {

    switch( SFG_TYPE ) {
    case sfheaders::sfg::SFG_POINT: {
      Rcpp::NumericVector vec = Rcpp::as< Rcpp::NumericVector >( sfg );
      return sfheaders::df::sfg_point_coordinates( vec, sfc_rows );
    }
    case sfheaders::sfg::SFG_MULTIPOINT: {
      Rcpp::NumericMatrix mat = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return sfheaders::df::sfg_multipoint_coordinates( mat, sfc_rows );
    }
    case sfheaders::sfg::SFG_LINESTRING: {
      Rcpp::NumericMatrix mat = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return sfheaders::df::sfg_linestring_coordinates( mat, sfc_rows );
    }
    case sfheaders::sfg::SFG_MULTILINESTRING: {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return sfheaders::df::sfg_multilinestring_coordinates( lst, sfc_rows );
    }
    case sfheaders::sfg::SFG_POLYGON: {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return sfheaders::df::sfg_polygon_coordinates( lst, sfc_rows );
    }
    case sfheaders::sfg::SFG_MULTIPOLYGON: {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return sfheaders::df::sfg_multipolygon_coordinates( lst, sfc_rows );
    }
    default: {
      Rcpp::stop("sfheaders - unknown sfg type");  // #nocov
    }
    }
    return Rcpp::List::create(); // #nocov never reaches
  }

  inline int get_sfg_type( std::string& sfg ) {
    if( sfg == "POINT" ) {
      return sfheaders::sfg::SFG_POINT;
    } else if ( sfg == "MULTIPOINT" ) {
      return sfheaders::sfg::SFG_MULTIPOINT;
    } else if ( sfg == "LINESTRING" ) {
      return sfheaders::sfg::SFG_LINESTRING;
    } else if ( sfg == "MULTILINESTRING" ) {
      return sfheaders::sfg::SFG_MULTILINESTRING;
    } else if ( sfg == "POLYGON" ) {
      return sfheaders::sfg::SFG_POLYGON;
    } else if ( sfg == "MULTIPOLYGON" ) {
      return sfheaders::sfg::SFG_MULTIPOLYGON;
    } else {
      Rcpp::stop("sfheaders - unknown sfg type");  // #nocov
    }
  }

  inline int get_sfg_column_index( std::string& sfg ) {
    if( sfg == "POINT" ) {
      return POINT_COLUMN;
    } else if ( sfg == "MULTIPOINT" ) {
      return MULTIPOINT_COLUMN;
    } else if ( sfg == "LINESTRING" ) {
      return LINESTRING_COLUMN;
    } else if ( sfg == "MULTILINESTRING" ) {
      return MULTILINESTRING_COLUMN;
    } else if ( sfg == "POLYGON" ) {
      return POLYGON_COLUMN;
    } else if ( sfg == "MULTIPOLYGON" ) {
      return MULTIPOLYGON_COLUMN;
    } else {
      Rcpp::stop("sfheaders - unknown sfg type");  // #nocov
    }
  }


  // used for any mixed geomtry, or non-POINT, because the total number of rows is variable
  inline Rcpp::List get_sfc_geometry_coordinates(
      Rcpp::List& sfc,
      R_xlen_t& total_coordinates
  ) {

    Rcpp::LogicalVector columns( MAX_COLUMNS ); // keeping track of which to subset
    columns[ X_COLUMN ] = true;
    columns[ Y_COLUMN ] = true;
    columns[ SFG_COLUMN ] = true;

    R_xlen_t n_sfg = sfc.size();
    R_xlen_t i;
    R_xlen_t j;
    R_xlen_t n_col;

    Rcpp::CharacterVector cls;
    std::string dim;
    std::string sfg_class;
    int sfg_type;
    int sfg_column_idx;

    R_xlen_t sfc_rows = 0;
    R_xlen_t total_rows = 0;

    double id;

    Rcpp::List res = setup_result( total_coordinates );

    for( i = 0; i < n_sfg; ++i ) {

      SEXP sfci = sfc[ i ];

      cls = sfheaders::utils::getSfgClass( sfci );

      dim = cls[0];

      if ( dim == "XYZM" ) {
        columns[ Z_COLUMN ] = true;
        columns[ M_COLUMN ] = true;
      } else if ( dim == "XYZ" ) {
        columns[ Z_COLUMN ] = true;
      } else if ( dim == "XYM" ) {
        columns[ M_COLUMN ] = true;
      }

      sfg_class = cls[1];
      sfg_type = get_sfg_type( sfg_class );
      sfg_column_idx = get_sfg_column_index( sfg_class );
      columns[ sfg_column_idx ] = true;

      Rcpp::List sfg = get_sfg_coordinates( sfci, sfc_rows, sfg_type );

      n_col = sfg.size();

      Rcpp::IntegerVector sfg_cols = get_sfg_cols( n_col, sfg_type, dim );
      column_index_check( sfg_cols, n_col );

      for( j = 0; j < n_col; ++j ) {

        Rcpp::NumericVector new_values_vector = sfg[ j ];
        int col_idx = sfg_cols[ j ];
        columns[ col_idx ] = true;
        Rcpp::NumericVector current_values_vector = res[ col_idx ];
        Rcpp::NumericVector result_vector = geometries::utils::fill_vector( current_values_vector, new_values_vector, total_rows );
        res[ col_idx ] = result_vector;
      }

      id = i + 1;
      Rcpp::NumericVector new_id_vector = Rcpp::rep( id, sfc_rows );
      Rcpp::NumericVector current_id_vector = res[ sfg_column_idx ];
      Rcpp::NumericVector filled = geometries::utils::fill_vector( current_id_vector, new_id_vector, total_rows );
      res[ sfg_column_idx ] = filled;

      Rcpp::NumericVector current_sfg_id_vector = res[ SFG_COLUMN ];
      filled = geometries::utils::fill_vector( current_sfg_id_vector, new_id_vector, total_rows );

      res[ SFG_COLUMN ] = filled;

      total_rows = total_rows + sfc_rows;
    }

    // make data.frame
    res = res[ columns ];
    Rcpp::StringVector res_names = column_names[ columns ];
    //Rcpp::StringVector res_names = column_names;
    return sfheaders::utils::make_dataframe( res, total_coordinates, res_names );
  }

  // for POINT geometries we can simplify / optimise because the number of rows is fixed
  inline Rcpp::List get_sfc_point_coordinates(
      Rcpp::List& sfc,
      R_xlen_t& total_coordinates
  ) {

    Rcpp::LogicalVector columns( 6 ); // keeping track of which to subset
    columns[ 2 ] = true; // x
    columns[ 3 ] = true; // y
    columns[ 0 ] = true; // sfg_id
    columns[ 1 ] = true; // point_id

    R_xlen_t n_sfg = sfc.size();
    R_xlen_t i;

    std::string dim;

    // TODO: change this
    // needs to be dependant on 'dim'
    Rcpp::StringVector col_names = {"sfg_id","point_id","x","y","z","m"};

    Rcpp::NumericVector x( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector y( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector z( total_coordinates, Rcpp::NumericVector::get_na() );
    Rcpp::NumericVector m( total_coordinates, Rcpp::NumericVector::get_na() );

    Rcpp::List res( 6 );

    for( i = 0; i < n_sfg; ++i ) {
      Rcpp::NumericVector sfg_point = sfc[ i ];

      Rcpp::CharacterVector cls = sfheaders::utils::getSfgClass( sfg_point );
      dim = cls[0];


      x[ i ] = sfg_point[ 0 ];
      y[ i ] = sfg_point[ 1 ];

      if( dim == "XYZM" ) {
        columns[ 4 ] = true;
        columns[ 5 ] = true;
        z[ i ] = sfg_point[ 2 ];
        m[ i ] = sfg_point[ 3 ];
      } else if ( dim == "XYM" ) {
        columns[ 5 ] = true;
        m[ i ] = sfg_point[ 2 ];
      } else if ( dim == "XYZ" ) {
        columns[ 4 ] = true;
        z[ i ] = sfg_point[ 2 ];
      }
    }

    // id columns "sfg_id" and "point_id", which will be 1:nrow(sfc);
    Rcpp::IntegerVector point_id = Rcpp::seq( 1, total_coordinates );
    Rcpp::IntegerVector sfg_id = Rcpp::seq( 1, total_coordinates );
    res[ 0 ] = sfg_id;
    res[ 1 ] = point_id;
    res[ 2 ] = x;
    res[ 3 ] = y;
    res[ 4 ] = z;
    res[ 5 ] = m;

    res = res[ columns ];
    Rcpp::StringVector res_names = col_names[ columns ];
    return sfheaders::utils::make_dataframe( res, total_coordinates, res_names );
  }

  inline Rcpp::List get_sfc_coordinates(
    Rcpp::List& sfc,
    R_xlen_t& total_coordinates
  ) {

    // issue 71
    if( !Rf_isNull( sfc.attr("class") ) ) {
      Rcpp::CharacterVector sfc_class = sfc.attr("class");
      std::string cls;
      cls = sfc_class[0];

      // switch on cls
      if ( cls == "sfc_POINT" ) {
        return get_sfc_point_coordinates( sfc, total_coordinates );
      }
    }

    return get_sfc_geometry_coordinates( sfc, total_coordinates );
  }

  inline Rcpp::List sfc_to_df(
      Rcpp::List& sfc,
      Rcpp::IntegerMatrix& sfc_coordinates
  ) {

    R_xlen_t n_geometries = sfc_coordinates.nrow();

    R_xlen_t total_coordinates = sfc_coordinates( n_geometries - 1 , 1 );
    total_coordinates = total_coordinates + 1;
    return get_sfc_coordinates( sfc, total_coordinates );
  }

  inline Rcpp::List sfc_to_df( Rcpp::List& sfc ) {

    // issue 71
    if( !Rf_isNull( sfc.attr("class") ) ) {
      // get teh sfc class here!
      // so if it's a POINT, can go direct to get_sfc_point()
      Rcpp::CharacterVector sfc_class = sfc.attr("class");
      std::string cls;
      cls = sfc_class[0];
      // switch on cls
      if ( cls == "sfc_POINT" ) {
        R_xlen_t n_geometries = sfc.size();
        return get_sfc_point_coordinates( sfc, n_geometries );
      }
    }

    // seprated this so it's independant / not called twice from `sf_to_df()`
    //return sfc;
    Rcpp::List dims = geometries::coordinates::geometry_dimensions( sfc );
    Rcpp::IntegerMatrix sfc_coordinates = dims["dimensions"];
    return sfc_to_df( sfc, sfc_coordinates );
  }

} // df
} // sfheaders


#endif
