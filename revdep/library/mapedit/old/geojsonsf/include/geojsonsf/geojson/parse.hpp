#ifndef GEOJSONSF_GEOJSON_PARSE_H
#define GEOJSONSF_GEOJSON_PARSE_H

#include <Rcpp.h>

#include "geometries/utils/attributes/attributes.hpp"

#include "geojsonsf/sf/sfg/geojson_sfg.hpp"
#include "geojsonsf/geojson/geojson_validate.hpp"

namespace geojsonsf {
namespace geojson {
namespace parse {

	inline void parse_geometry_object(
			Rcpp::List& sfc,
			R_xlen_t i,
	    const Value& geometry,
	    Rcpp::NumericVector& bbox,
	    Rcpp::NumericVector& z_range,
	    Rcpp::NumericVector& m_range,
	    std::unordered_set< std::string >& geometry_types,
	    R_xlen_t& sfg_objects
  ) {

		geojsonsf::validate::validate_type(geometry, sfg_objects);
		geojsonsf::validate::validate_coordinates(geometry, sfg_objects);
		geojsonsf::validate::validate_array(geometry["coordinates"], sfg_objects);

		std::string geom_type = geometry["type"].GetString();
		const Value& coord_array = geometry["coordinates"];
		geometry_types.insert( geom_type );


		if (geom_type == "Point") {
			geojsonsf::sfg::get_points( coord_array, bbox, z_range, m_range, sfc, i, true, "POINT");

		} else if (geom_type == "MultiPoint") {
			R_xlen_t max_cols = 2;
			geojsonsf::sfg::get_line_string( coord_array, bbox, z_range, m_range, sfc, i, true, "MULTIPOINT", max_cols );

		} else if (geom_type == "LineString") {
			R_xlen_t max_cols = 2;
			geojsonsf::sfg::get_line_string( coord_array, bbox, z_range, m_range, sfc, i, true, "LINESTRING", max_cols );

		} else if (geom_type == "MultiLineString") {
			geojsonsf::sfg::get_multi_line_string( coord_array, bbox, z_range, m_range, sfc, i, true, "MULTILINESTRING" );

		} else if (geom_type == "Polygon") {
			geojsonsf::sfg::get_polygon( coord_array, bbox, z_range, m_range, sfc, i, true, "POLYGON" );

		} else if (geom_type == "MultiPolygon") {
			geojsonsf::sfg::get_multi_polygon( coord_array, bbox, z_range, m_range, sfc, i, true, "MULTIPOLYGON" );

		} else {
			Rcpp::stop("unknown sfg type");
		}
	}

	inline Rcpp::List parse_geometry_collection_object(
			const Value& val,
	    Rcpp::NumericVector& bbox,
	    Rcpp::NumericVector& z_range,
	    Rcpp::NumericVector& m_range,
	    std::unordered_set< std::string >& geometry_types,
	    R_xlen_t& sfg_objects,
	    bool& expand_geometries
  ) {

		std::string geom_type;
		geojsonsf::validate::validate_geometries(val, sfg_objects);
		auto geometries = val["geometries"].GetArray();
		R_xlen_t n = geometries.Size();
		R_xlen_t i;
		Rcpp::List geom_collection(n);

		for (i = 0; i < n; ++i) {
			const Value& gcval = geometries[i];
			geojsonsf::validate::validate_type(gcval, sfg_objects);
			geom_type = gcval["type"].GetString();

			parse_geometry_object(geom_collection, i, gcval, bbox, z_range, m_range, geometry_types, sfg_objects);
		}
		geometry_types.insert( "GEOMETRYCOLLECTION" );

		if ( !expand_geometries ) {
			// TODO: check this dim; should it be set as XY?
			Rcpp::StringVector class_attribute = { "XY", "GEOMETRYCOLLECTION","sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( geom_collection, atts );
		} else {
			sfg_objects+=n;
		}
		return geom_collection;
	}

	inline Rcpp::List parse_feature_object(
			const Value& feature,
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

		geojsonsf::validate::validate_geometry(feature, sfg_objects);

		Rcpp::List sfc(1);

		const Value& geometry = feature["geometry"];
		std::string type;

		if (geometry.Size() > 0) {

			geojsonsf::validate::validate_type( geometry, sfg_objects );
			type = geometry["type"].GetString();

			if (type == "GeometryCollection") {
				sfc[0] = geojsonsf::geojson::parse::parse_geometry_collection_object(geometry, bbox, z_range, m_range, geometry_types, sfg_objects, expand_geometries);
			} else {
				geojsonsf::geojson::parse::parse_geometry_object(sfc, 0, geometry, bbox, z_range, m_range, geometry_types, sfg_objects);
			}
		} else {
			geojsonsf::sfg::create_null_sfg(sfc, geometry_types, nempty);
		}

		if (type != "GeometryCollection") {
			sfg_objects++;
		} else if (type == "GeometryCollection" && !expand_geometries){
			sfg_objects++;
		}

		const Value& p = feature["properties"];

		geojsonsf::geojson_properties::get_property_keys(p, property_keys);
		geojsonsf::geojson_properties::get_property_types(p, property_types);

		R_xlen_t geomsize = 1;
		R_xlen_t i;

		if (expand_geometries && type == "GeometryCollection") {
			geojsonsf::validate::validate_geometries( geometry, sfg_objects );
			auto geometries = geometry["geometries"].GetArray();
			geomsize = geometries.Size();
		}

		std::string s;
		for ( i = 0; i < geomsize; ++i ) {
			//https://stackoverflow.com/a/33473321/5977215
			if ( expand_geometries ) {
				s = std::to_string( sfg_objects - i );
			} else {
				s = std::to_string( sfg_objects );
			}
			Value n( s.c_str(), doc_properties.GetAllocator() );

			// TODO: is this method deep-cloning?
			Value properties( feature["properties"], doc_properties.GetAllocator() );
			doc_properties.AddMember( n, properties, doc_properties.GetAllocator() );

		}

		return sfc;
	}

	inline Rcpp::List parse_feature_collection_object(
			const Value& fc,
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

		// a FeatureCollection MUST have members (array) called features,
		geojsonsf::validate::validate_features(fc, sfg_objects);


		auto features = fc["features"].GetArray();

		R_xlen_t n = features.Size(); // number of features
		R_xlen_t i;

		Rcpp::List feature_collection(n);

		for ( i = 0; i < n; ++i ) {
			const Value& feature = features[i];
			feature_collection[i] = parse_feature_object(
				feature, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys, doc_properties,
				property_types, expand_geometries, nempty
			);
		}
		return feature_collection;
	}

	inline void parse_geojson(
			const Value& v,
	    Rcpp::List& sfc,
	    Rcpp::List& properties,
	    R_xlen_t i,
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

		Rcpp::List res(1);
		geojsonsf::validate::validate_type(v, sfg_objects);

		std::string geom_type = v["type"].GetString();

		if (geom_type == "Feature") {
			res = parse_feature_object(v, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys, doc_properties, property_types, expand_geometries, nempty);
			sfc[i] = res;

		} else if (geom_type == "FeatureCollection") {

			res = parse_feature_collection_object(v, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys, doc_properties, property_types, expand_geometries, nempty);
			sfc[i] = res;

		} else if (geom_type == "GeometryCollection") {

			res = parse_geometry_collection_object(v, bbox, z_range, m_range, geometry_types, sfg_objects, expand_geometries);
			if (!expand_geometries) {
				sfg_objects++;
			}
			sfc[i] = res;

		} else {

			parse_geometry_object(sfc, i, v, bbox, z_range, m_range, geometry_types, sfg_objects);
			sfg_objects++;
		}
	}

	inline void parse_geojson_object(
			Document& d,
	    Rcpp::List& sfc,
	    Rcpp::List& properties,
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
		const Value& v = d;
		parse_geojson(
			v, sfc, properties, 0, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
			doc_properties, property_types, expand_geometries, nempty
		);

		// // out of order
		// for ( auto it = property_keys.begin(); it != property_keys.end(); it++ ) {
		// 	//const char s = *it->c_str();
		// 	std::cout << (*it) << std::endl;
		// }

	}

	inline void parse_geojson_array(
			Document& d,
	    Rcpp::List& sfc,
	    Rcpp::List& properties,
	    R_xlen_t i,
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
		const Value& v = d[i];
		parse_geojson(
			v, sfc, properties, i, bbox, z_range, m_range, geometry_types, sfg_objects, property_keys,
			doc_properties, property_types, expand_geometries, nempty
		);
	}

} // namespace parse
} // namespace geojson
} // namespace geojsonsf

#endif
