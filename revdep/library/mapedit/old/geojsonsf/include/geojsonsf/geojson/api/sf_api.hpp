#ifndef GEOJSONSF_GEOJSON_SF_API_H
#define GEOJSONSF_GEOJSON_SF_API_H

#include "jsonify/jsonify.hpp"
#include "jsonify/to_json/writers/simple.hpp"

#include "geojsonsf/geojsonsf.h"
#include "geojsonsf/geojson/write_geometry.hpp"

namespace geojsonsf {
namespace api {

  inline Rcpp::StringVector sfc_to_geojson(
        Rcpp::List& sfc,
        int& digits
  ) {
  	// atomise - each row is a separate GeoJSON string

  	R_xlen_t n_rows = sfc.size();
   R_xlen_t i;

  	Rcpp::StringVector geojson( n_rows );

  	for( i = 0; i < n_rows; ++i ) {

  		rapidjson::StringBuffer sb;
  		rapidjson::Writer < rapidjson::StringBuffer > writer( sb );

  		geojsonsf::write_geometry::write_geometry( writer, sfc, i, digits );
  		geojson[i] = sb.GetString();
  	}
  	geojsonsf::attach_class( geojson );
  	return geojson;
  }

  /*
   * sf_to_geojson
   *
   * Converts sf object to GeoJSON
   */
  inline Rcpp::StringVector sf_to_geojson(
        Rcpp::DataFrame& sf,
        int& digits,
        bool& factors_as_string
  ) {
  	rapidjson::StringBuffer sb;
  	rapidjson::Writer < rapidjson::StringBuffer > writer( sb );

  	std::string geom_column = sf.attr("sf_column");

  	R_xlen_t n_cols = sf.ncol();
  	R_xlen_t n_properties = n_cols - 1;
  	R_xlen_t n_rows = sf.nrows();
  	R_xlen_t i, j;
  	Rcpp::StringVector column_names = sf.names();
  	Rcpp::StringVector property_names(sf.size() - 1);

  	R_xlen_t property_counter = 0;
  	for ( i = 0; i < sf.length(); ++i) {
  		if (column_names[i] != geom_column) {
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

  			SEXP this_vec = sf[ h ];
  			writer.String( h );
  			jsonify::writers::simple::write_value( writer, this_vec, i, -1, false, factors_as_string  );
  		}
  		writer.EndObject();

  		writer.String("geometry");

  		Rcpp::List sfc = sf[ geom_column ];
  		geojsonsf::write_geometry::write_geometry( writer, sfc, i, digits );

  		writer.EndObject();
  	}

  	writer.EndArray();
  	writer.EndObject();

  	Rcpp::StringVector geojson = sb.GetString();
  	geojsonsf::attach_class( geojson );
  	return geojson;
  }

  /*
   *
   * sf_to_geojson_atomise
   *
   * Takes an sf object, converts to atomised GeoJSON
   * Where every geometry is turned into an individual array
   */
  inline Rcpp::StringVector sf_to_geojson_atomise(
        Rcpp::DataFrame& sf,
        int& digits,
        bool& factors_as_string
  ) {
  	// atomise - each row is a separate GeoJSON string

   std::string geom_column = sf.attr("sf_column");

   R_xlen_t n_cols = sf.ncol();
   R_xlen_t n_properties = n_cols - 1;
   R_xlen_t n_rows = sf.nrows();
R_xlen_t i, j;
  	Rcpp::StringVector column_names = sf.names();
  	Rcpp::StringVector property_names(sf.size() - 1);

  	Rcpp::StringVector geojson( n_rows );

  	R_xlen_t property_counter = 0;
  	for (i = 0; i < sf.length(); ++i) {
  		if (column_names[i] != geom_column) {
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

  				SEXP this_vec = sf[ h ];

  				writer.String( h );
  				jsonify::writers::simple::write_value( writer, this_vec, i, -1, false, factors_as_string  );
  			}
  			writer.EndObject();
  		}

  		// now geometries
  		if( n_properties > 0 ) {
  			writer.String("geometry");
  		}

  		Rcpp::List sfc = sf[ geom_column ];
  		geojsonsf::write_geometry::write_geometry( writer, sfc, i, digits );

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
