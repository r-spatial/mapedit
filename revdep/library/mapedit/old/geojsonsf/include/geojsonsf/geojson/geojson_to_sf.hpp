#ifndef GEOJSON_TO_SF_H
#define GEOJSON_TO_SF_H


#include <algorithm>
#include <Rcpp.h>
#include "rapidjson/document.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/writer.h"

#include "geojsonsf/geojsonsf.h"
#include "geojsonsf/sf/sf/construct.hpp"
#include "geojsonsf/sf/sfc/geojson_sfc.hpp"
#include "geojsonsf/sf/sfg/geojson_sfg.hpp"
#include "geojsonsf/geojson/geojson_validate.hpp"
#include "geojsonsf/geojson/geojson_properties.hpp"
#include "geojsonsf/geojson/parse.hpp"

#include "geometries/bbox/bbox.hpp"
//#include "sfheaders/sfc/bbox.hpp"

#include "sfheaders/sfc/zm_range.hpp"

using namespace rapidjson;

namespace geojsonsf {
namespace sf {

  inline Rcpp::List geojson_to_sf(
  	Document& d,
  	Rcpp::NumericVector& bbox,
  	Rcpp::NumericVector& z_range,
  	Rcpp::NumericVector& m_range,
  	std::unordered_set< std::string >& geometry_types,
  	R_xlen_t& sfg_objects,
  	std::unordered_set< std::string >& property_keys,
  	Document& doc_properties,
  	std::unordered_map< std::string, std::string>& property_types,
  	bool& expand_geometries,
  	R_xlen_t& nempty
  ) {
  	Rcpp::List sf(1);
  	Rcpp::List sfc(1);
  	Rcpp::List properties(1);
  	R_xlen_t doc_ele;

  	if (d.IsObject()) {
  		Rcpp::List sfg(1);
  		geojsonsf::geojson::parse::parse_geojson_object(
  			d, sfg, properties, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
  			doc_properties, property_types, expand_geometries, nempty
  		);
  		sfc[0] = sfg;

  	} else if (d.IsArray()) {

  		Rcpp::List sfgs(d.Size());

  		for (doc_ele = 0; doc_ele < d.Size(); doc_ele++) {
  			geojsonsf::geojson::parse::parse_geojson_array(
  				d, sfgs, properties, doc_ele, bbox, z_range, m_range, geometry_types, sfg_objects,
  				property_keys, doc_properties, property_types, expand_geometries, nempty
  			);
  		}
  		sfc[0] = sfgs;
  	}
  	return sfc;
	}

	inline Rcpp::List geojson_to_sf(
			const char* geojson,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			std::unordered_set< std::string >& geometry_types,
			R_xlen_t& sfg_objects,
			std::unordered_set< std::string >& property_keys,
			Document& doc_properties,
			std::unordered_map< std::string, std::string>& property_types,
			bool& expand_geometries,
			R_xlen_t& nempty
	) {

		Document d;
		geojsonsf::validate::safe_parse(d, geojson);

		return geojson_to_sf(
			d, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys, doc_properties, property_types,
			expand_geometries, nempty
		);
	}

	inline Rcpp::List create_sfc(
			Document& d,
			bool& expand_geometries
	) {
		// iterate over the geojson

		R_xlen_t sfg_objects = 0;  // keep track of number of objects
		R_xlen_t nempty = 0;
		//int row_index;

		// Attributes to keep track of along the way
		Rcpp::NumericVector bbox = geojsonsf::sfc::utils::start_bbox();
		Rcpp::NumericVector z_range = geojsonsf::sfc::utils::start_zm_range();
		Rcpp::NumericVector m_range = geojsonsf::sfc::utils::start_zm_range();

		std::unordered_set< std::string > geometry_types;
		std::unordered_set< std::string > property_keys;   // storing all the 'key' values from 'properties'
		std::unordered_map< std::string, std::string> property_types;

		Document doc_properties;    // Document to store the 'properties'
		doc_properties.SetObject();

		Rcpp::List sfc = geojson_to_sf(
			  d, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
				doc_properties, property_types, expand_geometries, nempty
			);

		return geojsonsf::sfc::construct_sfc(sfg_objects, sfc, bbox, z_range, m_range, geometry_types, nempty);
	}

  inline Rcpp::List create_sfc(
  		Rcpp::StringVector geojson, bool& expand_geometries
  ) {
		// iterate over the geojson
		int n = geojson.size();
  	R_xlen_t sfg_objects = 0;  // keep track of number of objects
		R_xlen_t nempty = 0;
		//int row_index;

		// Attributes to keep track of along the way
		Rcpp::NumericVector bbox = geojsonsf::sfc::utils::start_bbox();
		Rcpp::NumericVector z_range = geojsonsf::sfc::utils::start_zm_range();
		Rcpp::NumericVector m_range = geojsonsf::sfc::utils::start_zm_range();

		std::unordered_set< std::string > geometry_types;
		std::unordered_set< std::string > property_keys;   // storing all the 'key' values from 'properties'
		std::unordered_map< std::string, std::string> property_types;

		Document doc_properties;    // Document to store the 'properties'
		doc_properties.SetObject();
		Rcpp::List sfc(n);

		for (int geo_ele = 0; geo_ele < n; geo_ele++ ){
			sfc[geo_ele] = geojson_to_sf(
				geojson[geo_ele], bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
				doc_properties, property_types, expand_geometries, nempty
			);
		}

		return geojsonsf::sfc::construct_sfc(sfg_objects, sfc, bbox, z_range, m_range, geometry_types, nempty);
	}

  inline Rcpp::List generic_geojson_to_sf(
  		Document& d,
  		bool& expand_geometries
  ) {

  	R_xlen_t sfg_objects = 0;  // keep track of number of objects
  	R_xlen_t row_index = 0;
  	R_xlen_t nempty = 0;

  	Rcpp::NumericVector bbox = geojsonsf::sfc::utils::start_bbox();
  	Rcpp::NumericVector z_range = geojsonsf::sfc::utils::start_zm_range();
  	Rcpp::NumericVector m_range = geojsonsf::sfc::utils::start_zm_range();

  	std::unordered_set< std::string > geometry_types;
  	std::unordered_set< std::string > property_keys;   // storing all the 'key' values from 'properties'
  	std::unordered_map< std::string, std::string > property_types;

  	Document doc_properties;    // Document to store the 'properties'
  	doc_properties.SetObject();
  	// Rcpp::List sfc( 1 );

  	Rcpp::List sfc = geojson_to_sf(
  		d, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys, doc_properties,
  		property_types, expand_geometries, nempty
  	);

  	Rcpp::List res = geojsonsf::sfc::construct_sfc( sfg_objects, sfc, bbox, z_range, m_range, geometry_types, nempty );
  	return geojsonsf::sf::construct_sf( res, property_keys, property_types, doc_properties, sfg_objects, row_index );

  	//return Rcpp::List::create();
  }

  inline Rcpp::List generic_geojson_to_sf(
  		Rcpp::StringVector geojson,
  		bool& expand_geometries
  ) {
		// iterate over the geojson
		int n = geojson.size();
  	R_xlen_t sfg_objects = 0;  // keep track of number of objects
  	R_xlen_t row_index = 0;
		R_xlen_t nempty = 0;

		// Attributes to keep track of along the way
		Rcpp::NumericVector bbox = geojsonsf::sfc::utils::start_bbox();
		Rcpp::NumericVector z_range = geojsonsf::sfc::utils::start_zm_range();
		Rcpp::NumericVector m_range = geojsonsf::sfc::utils::start_zm_range();

		std::unordered_set< std::string > geometry_types;
		std::unordered_set< std::string > property_keys;   // storing all the 'key' values from 'properties'
		std::unordered_map< std::string, std::string > property_types;

		Document doc_properties;    // Document to store the 'properties'
		doc_properties.SetObject();
		Rcpp::List sfc( n );

		for ( int geo_ele = 0; geo_ele < n; geo_ele++ ){
			sfc[geo_ele] = geojson_to_sf(
				geojson[geo_ele], bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
				doc_properties, property_types, expand_geometries, nempty
			);
		}

		Rcpp::List res = geojsonsf::sfc::construct_sfc( sfg_objects, sfc, bbox, z_range, m_range, geometry_types, nempty );
		return geojsonsf::sf::construct_sf( res, property_keys, property_types, doc_properties, sfg_objects, row_index );
	}

} // namesapce sf
} // namespace geojsonsf

#endif
