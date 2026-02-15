#ifndef GEOJSONSF_GEOJSON_DF_API_H
#define GEOJSONSF_GEOJSON_DF_API_H

#include "jsonify/jsonify.hpp"
#include "jsonify/to_json/writers/simple.hpp"

#include "geojsonsf/geojsonsf.h"
#include "geojsonsf/geojson/write_geometry.hpp"

#include "sfheaders/sfg/sfg_dimension.hpp"

//#include "sfheaders/utils/vectors/vectors.hpp"

#include "geometries/utils/vectors/vectors.hpp"

namespace geojsonsf {
namespace api {

  inline Rcpp::StringVector df_to_geojson(
  		Rcpp::DataFrame& df,
  		Rcpp::StringVector& geometry_columns,
  		int& digits,
  		bool& factors_as_string
    ) {

  	rapidjson::StringBuffer sb;
  	rapidjson::Writer < rapidjson::StringBuffer > writer( sb );

  	R_xlen_t n_cols = df.ncol();
  	R_xlen_t n_rows = df.nrows();
  	R_xlen_t i, j;
  	Rcpp::StringVector column_names = df.names();

  	// the sfc_POINT
  	R_xlen_t n_geometry_columns = geometry_columns.size();
  	Rcpp::List geometry_vectors( n_geometry_columns );

  	R_xlen_t n_properties = n_cols - n_geometry_columns;
  	Rcpp::StringVector property_names( n_properties );

  	for( i = 0; i < n_geometry_columns; ++i ) {
  		Rcpp::String this_geometry = geometry_columns[i];
  		geometry_vectors[i] = df[ this_geometry ];
  	}


  	std::string dim = sfheaders::sfg::sfg_dimension( n_geometry_columns );
  	Rcpp::CharacterVector cls = Rcpp::CharacterVector::create( dim , "POINT", "sfg");

  	R_xlen_t property_counter = 0;

  	for ( i = 0; i < df.length(); ++i ) {

  		Rcpp::String this_column = column_names[i];
  	  R_xlen_t idx = geometries::utils::where_is( this_column, geometry_columns );

  		if ( idx == -1 ) {  // i.e. it's not in the vector
  			property_names[property_counter] = column_names[i];
  			property_counter++;
  		}
  	}

  	writer.StartObject();
  	geojsonsf::writers::start_feature_collection( writer );

  	writer.StartArray();

  	for( i = 0; i < n_rows; ++i ) {

  		writer.StartObject();

  		geojsonsf::writers::start_features( writer );
  		geojsonsf::writers::start_properties( writer );
  		writer.StartObject();
  		// properties first, then sfc

  		for( j = 0; j < n_properties; ++j ) {
  			const char *h = property_names[ j ];
  			SEXP this_vec = df[ h ];

  			writer.String( h );
  			jsonify::writers::simple::write_value( writer, this_vec, i, -1, false, factors_as_string );
  		}
  		writer.EndObject();

  		writer.String("geometry");

  		Rcpp::NumericVector geom( n_geometry_columns );
  		for ( j = 0; j < n_geometry_columns; ++j ) {
  			Rcpp::NumericVector this_geometry_vector = geometry_vectors[j];
  			geom[j] = this_geometry_vector[i];
  		}
  		SEXP sfg = geom;
  		geojsonsf::write_geometry::write_geometry( writer, sfg, cls, digits );

  		writer.EndObject();
  	}

  	writer.EndArray();
  	writer.EndObject();

  	Rcpp::StringVector geojson = sb.GetString();
  	geojsonsf::attach_class( geojson );
  	return geojson;
  }

  inline Rcpp::StringVector df_to_geojson_atomise(
  		Rcpp::DataFrame& df,
  		Rcpp::StringVector& geometry_columns,
  		int& digits,
  		bool& factors_as_string
  ) {

    R_xlen_t n_cols = df.ncol();
    R_xlen_t n_rows = df.nrows();
    R_xlen_t i, j;
  	Rcpp::StringVector column_names = df.names();

  	Rcpp::StringVector geojson( n_rows );


  	R_xlen_t n_geometry_columns = geometry_columns.size();
  	Rcpp::List geometry_vectors( n_geometry_columns );

  	R_xlen_t n_properties = n_cols - n_geometry_columns;
  	Rcpp::StringVector property_names( n_properties );

  	for ( i = 0; i < n_geometry_columns; ++i ) {
  		Rcpp::String this_geometry = geometry_columns[i];
  		geometry_vectors[i] = df[ this_geometry ];
  	}

  	std::string dim = sfheaders::sfg::sfg_dimension( n_geometry_columns );
  	Rcpp::CharacterVector cls = Rcpp::CharacterVector::create( dim , "POINT", "sfg");

  	R_xlen_t property_counter = 0;
  	for ( i = 0; i < df.length(); ++i) {

  		Rcpp::String this_column = column_names[i];
  	  R_xlen_t idx = geometries::utils::where_is( this_column, geometry_columns );

  		if ( idx == -1 ) {  // i.e. it's not in the vector

  			property_names[property_counter] = column_names[i];
  			property_counter++;
  		}
  	}


  	for( i = 0; i < n_rows; ++i ) {

  		rapidjson::StringBuffer sb;
  		rapidjson::Writer < rapidjson::StringBuffer > writer( sb );

  		if ( n_properties > 0 ) {
  			writer.StartObject();
  			geojsonsf::writers::start_features( writer );
  			geojsonsf::writers::start_properties( writer );

  			writer.StartObject();

  			// properties first, then sfc
  			for( j = 0; j < n_properties; ++j ) {
  				const char *h = property_names[ j ];

  				SEXP this_vec = df[ h ];

  				writer.String( h );
  				jsonify::writers::simple::write_value( writer, this_vec, i, -1, false, factors_as_string  );
  			}
  			writer.EndObject();
  		}

  		// now geometries
  		if( n_properties > 0 ) {
  			writer.String("geometry");
  		}

  		Rcpp::NumericVector geom( n_geometry_columns );
  		for ( j = 0; j < n_geometry_columns; ++j ) {
  			Rcpp::NumericVector this_geometry_vector = geometry_vectors[j];
  			geom[j] = this_geometry_vector[i];
  		}
  		SEXP sfg = geom;
  		geojsonsf::write_geometry::write_geometry( writer, sfg, cls, digits );

  		if( n_properties > 0 ) {
  			writer.EndObject();
  		}
  		geojson[i] = sb.GetString();
  	}

  	geojsonsf::attach_class( geojson );
  	return geojson;
  }

} // namespace geojsonsf
} // namespace api


#endif
