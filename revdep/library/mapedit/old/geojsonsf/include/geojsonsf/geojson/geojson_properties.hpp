#ifndef GEOJSONSF_PROPERTIES_H
#define GEOJSONSF_PROPERTIES_H

#include "rapidjson/document.h"
#include <Rcpp.h>

using namespace rapidjson;

namespace geojsonsf {
namespace geojson_properties {

	template <typename T>
	inline std::string any_to_string(const T& obj) {
		std::stringstream ss;
		ss << obj;
		return ss.str();
	}

	/*
	 * Updates a list element to string
	 */
	inline void vector_to_string(
			Rcpp::List& lst,
			std::string& key
		) {
		Rcpp::StringVector sv = Rcpp::as<Rcpp::StringVector>(lst[key]);
		lst[key] = sv;
	}

	inline void get_property_types(
			const Value& v,
			std::unordered_map< std::string, std::string>& property_types
		) {

		// TODO: move to a header??
		static const char* ARRAY_TYPES[] =
			{ "Null", "False", "True", "Object", "Array", "String", "Number" };

		for (Value::ConstMemberIterator iter = v.MemberBegin(); iter != v.MemberEnd(); ++iter) {
			std::string property = iter->name.GetString();

			std::string type = ARRAY_TYPES[iter->value.GetType()];

			// check if key exists
			if (property_types.count(property) == 1) {
				// it exists
				std::string existing_type = property_types[property];

				if (existing_type == "String") {
					// if it's already a 'String' (JSON type), exit

				} else if (existing_type != "Null" && existing_type != type && type != "Null") {

					// allow NULL through so the type is correct when back in R
					// Treat "True" and "False" as the same type (logical)
					bool both_logical = (existing_type == "True" || existing_type == "False") &&
					                    (type == "True" || type == "False");

					if (!both_logical) {
						// if it's different, update to be a 'String'
						property_types[property] = "String";
					}

				} else if (existing_type == "Null") {
					// If the first element is NULL, use the new type
					property_types[property] = type;
				}
				// if it's not different, exit
			} else {
				// doesn't exist
				property_types[property] = type;

			}
			// if it doesn't exist, add the key / type to the map
		}
	}


	inline void sort_property_names(
			Rcpp::List& properties,
			std::unordered_set< std::string >& property_keys
		) {

		properties.names() = property_keys;
		std::vector< std::string > n = properties.names();
		std::reverse( n.begin(), n.end() );
		Rcpp::StringVector sv( n.size() );
		R_xlen_t i;

		for( i = 0; i < n.size(); ++i ) {
			std::string s = n[i];
			sv[i] = Rcpp::String( s );
		}
		properties.names() = sv;
	}

	inline void get_property_keys(
			const Value& v,
			std::unordered_set< std::string >& property_keys
		) {

		for ( Value::ConstMemberIterator iter = v.MemberBegin(); iter != v.MemberEnd(); ++iter ) {

			//   	std::string s = iter->name.GetString();
			//   	Rcpp::Rcout << s << std::endl;

			property_keys.insert(iter->name.GetString());
		}
		//   for ( auto it = property_keys.begin(); it != property_keys.end(); it++ ) {
		//   	Rcpp::Rcout << (*it) << std::endl;
		//   }
	}

	inline void update_string_vector(
			Rcpp::List& sf,
			std::string& key,
			const std::string& value,
			const R_xlen_t& row_index
		) {
		Rcpp::StringVector sv = sf[key];
		sv[row_index] = Rcpp::String( value );
		sf[key] = sv;
	}

	inline void update_logical_vector(
			Rcpp::List& sf,
			std::string& key,
			const bool& value,
			const R_xlen_t& row_index
		) {
		Rcpp::LogicalVector lv = sf[key];
		lv[row_index] = value;
		sf[key] = lv;
	}

	inline void update_numeric_vector(
			Rcpp::List& sf,
			std::string& key,
			double& value,
			const R_xlen_t& row_index
		) {
		Rcpp::NumericVector nv = sf[key];
		nv[row_index] = value;
		sf[key] = nv;
	}

	inline Rcpp::NumericVector na_numeric_vector(
			const R_xlen_t& n_elements
		) {
		Rcpp::NumericVector nv(n_elements, NA_REAL);
		return nv;
	}

	inline Rcpp::StringVector na_string_vector(
			const R_xlen_t& n_elements
		) {
		Rcpp::StringVector sv(n_elements, Rcpp::StringVector::get_na());
		return sv;
	}

	inline Rcpp::LogicalVector na_logical_vector(
			const R_xlen_t& n_elements
		) {
		Rcpp::LogicalVector lv(n_elements, NA_LOGICAL);
		return lv;
	}

	inline void setup_property_vectors(
			std::unordered_map< std::string, std::string>& property_types,
      Rcpp::List& properties,
      R_xlen_t& sfg_objects
		) {

		for (auto keys_it = property_types.begin();  keys_it != property_types.end(); keys_it++ ) {

			std::string this_key = keys_it->first;
			std::string this_type = keys_it->second;

			if (this_type == "False" || this_type == "True" ) {
				properties[ this_key ] = na_logical_vector( sfg_objects );
			} else if (this_type == "Number") {
				properties[ this_key ] = na_numeric_vector( sfg_objects );
			} else {
				properties[ this_key ] = na_string_vector( sfg_objects );
			}
		}
	}

	inline void nested_json_to_string(
			rapidjson::Value& v,
      std::string& type,
      Rcpp::List& properties,
      R_xlen_t& row_index,
      std::string& key
		) {

		StringBuffer sb;
		Writer<StringBuffer> writer(sb);
		v.Accept(writer);
		std::string this_value = sb.GetString();

		if (type != "String") {
			std::string value = any_to_string(this_value);
			update_string_vector(properties, key, value, row_index -1);
		} else {
			std::string value = this_value;
			update_string_vector(properties, key, value, row_index -1);
		}
	}

	inline void fill_property_vectors(
			Document& doc_properties,
      std::unordered_map< std::string, std::string>& property_types,
      Rcpp::List& properties,
      R_xlen_t& row_index
		) {

		// TODO(move to header):
		static const char* ARRAY_TYPES[] =
			{ "Null", "False", "True", "Object", "Array", "String", "Number" };

		for (auto& m : doc_properties.GetObject() ) {
			row_index = std::stoi( m.name.GetString() );

			for (auto& p : m.value.GetObject() ) {

				std::string key = p.name.GetString();
				std::string type = property_types[ key ];

				std::string value_type = ARRAY_TYPES[p.value.GetType()];

				if (value_type == "String") {
					std::string this_value = p.value.GetString();

					if (type != "String") {
						std::string value = any_to_string(this_value);
						update_string_vector(properties, key, value, row_index-1);
					} else {
						std::string value = this_value;
						update_string_vector(properties, key, value, row_index-1);
					}

				} else if (value_type == "Number") {

					double this_value = p.value.GetDouble();

					if (type != "Number") {
						std::string value = any_to_string(this_value);
						update_string_vector(properties, key, value, row_index-1);
					} else {
						double value = p.value.GetDouble();
						update_numeric_vector(properties, key, value, row_index-1);
					}

				} else if (value_type == "False" || value_type == "True") {

					bool this_value = p.value.GetBool();
					if (type != "True" && type != "False") {
						std::string value = any_to_string(this_value);
						update_string_vector(properties, key, value, row_index-1);
					} else {
						bool value = p.value.GetBool();
						update_logical_vector(properties, key, value, row_index-1);
					}

				} else if (value_type == "Null") {
					// don't do anything...
				} else if (value_type == "Object") {

					Value& v = p.value.GetObject();
					nested_json_to_string(v, type, properties, row_index, key);

				} else if (value_type == "Array") {

					Value& v = p.value.GetArray();
					nested_json_to_string(v, type, properties, row_index, key);

				} else {
					Rcpp::stop("unknown column data type " + type);
				}
			}
		}
	}


} // namespace geojson
} // namespace geojsonsf

#endif
