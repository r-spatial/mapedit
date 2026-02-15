#ifndef GEOJSONSF_SF_CONSTRUCTORS_H
#define GEOJSONSF_SF_CONSTRUCTORS_H

#include <Rcpp.h>
#include "geojsonsf/geojson/geojson_properties.hpp"

namespace geojsonsf {
namespace sf {

	inline Rcpp::List construct_sf(
			Rcpp::List& lst, std::unordered_set< std::string >& property_keys,
	    std::unordered_map< std::string, std::string>& property_types,
	    Document& doc_properties,
	    R_xlen_t& sfg_objects,
	    R_xlen_t& row_index
  ) {

		int n_cols = property_keys.size();
		if ( sfg_objects > 0 ) {
			property_keys.insert("geometry");
			n_cols++;  // expand to include geometry
		}

		Rcpp::List properties( n_cols );

		geojsonsf::geojson_properties::sort_property_names(properties, property_keys);

		properties["geometry"] = lst;

		geojsonsf::geojson_properties::setup_property_vectors( property_types, properties, sfg_objects );
		geojsonsf::geojson_properties::fill_property_vectors( doc_properties, property_types, properties, row_index );

		if (sfg_objects > 0 ) {
			Rcpp::IntegerVector nv = Rcpp::seq( 1, sfg_objects );
			properties.attr("row.names") = nv;
		} else {
			properties.attr("row.names") = Rcpp::IntegerVector(0);
		}
		properties.attr("class") = Rcpp::CharacterVector::create("sf", "data.frame");
		properties.attr("sf_column") = "geometry";

		return properties;
	}

} // namespace sf
} // namespace geojsonsf


#endif
