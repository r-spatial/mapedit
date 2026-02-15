#ifndef GEOJSONSF_SFC_H
#define GEOJSONSF_SFC_H

#include <Rcpp.h>

#include "geojsonsf/sf/sfc/utils/sfc_utils.hpp"

#include "sfheaders/sfc/sfc_attributes.hpp"

namespace geojsonsf {
namespace sfc {

	inline Rcpp::List construct_sfc(
			R_xlen_t& sfg_objects,
	    Rcpp::List& sf,
	    Rcpp::NumericVector& bbox,
	    Rcpp::NumericVector& z_range,
	    Rcpp::NumericVector& m_range,
	    std::unordered_set< std::string >& geometry_types,
	    R_xlen_t& nempty
  ) {

		Rcpp::List sfc_output( sfg_objects );
		std::string geom_attr;

		R_xlen_t sfg_counter = 0;

		Rcpp::List crs = Rcpp::List::create(
			Rcpp::_["input"] = geojsonsf::INPUT,
			Rcpp::_["wkt"] = geojsonsf::WKT
		);

		geojsonsf::sfc::utils::fetch_geometries( sf, sfc_output, sfg_counter );
		sfheaders::sfc::attach_sfc_attributes(
			sfc_output, geom_attr, geometry_types, bbox, z_range, m_range, crs, nempty
			);

		return sfc_output;
	}

} // namespace sfc
} // namespace geojsonsf

#endif
