#ifndef GEOJSONSF_SFC_UTILS_H
#define GEOJSONSF_SFC_UTILS_H

#include "geojsonsf/geojsonsf.h"

#include "geometries/bbox/bbox.hpp"

#include "sfheaders/sfc/sfc_attributes.hpp"

namespace geojsonsf {
namespace sfc {
namespace utils {

	inline Rcpp::NumericVector start_bbox() {
		Rcpp::NumericVector bbox(4);  // xmin, ymin, xmax, ymax
		bbox(0) = bbox(1) = bbox(2) = bbox(3) = NA_REAL;
		return bbox;
	}

	inline Rcpp::NumericVector start_zm_range() {
		Rcpp::NumericVector range(2);
		range(0) = range(1) = NA_REAL;
		return range;
	}

	inline Rcpp::List create_null_sfc() {
		Rcpp::List empty_sfc(0);

		std::string type = "GEOMETRY";
		Rcpp::NumericVector bbox = start_bbox();
		Rcpp::NumericVector z_range = start_zm_range();
		Rcpp::NumericVector m_range = start_zm_range();
		int n_empty = 0;
		std::unordered_set< std::string > geometry_types{"GEOMETRY"};

		// int epsg = geojsonsf::EPSG;
		//std::string proj = geojsonsf::PROJ4STRING;
		// Rcpp::String proj = geojsonsf::PROJ4STRING;

		Rcpp::List crs = Rcpp::List::create(
			Rcpp::_["input"] = geojsonsf::INPUT,
			Rcpp::_["wkt"] = geojsonsf::WKT
		);

		sfheaders::sfc::attach_sfc_attributes(
			empty_sfc, type, geometry_types, bbox, z_range, m_range, crs, n_empty
			);
		return empty_sfc;
	}

	inline void fetch_geometries(
			Rcpp::List& sf,
			Rcpp::List& res,
			R_xlen_t& sfg_counter
  ) {

		std::string geom_attr;

		for (Rcpp::List::iterator it = sf.begin(); it != sf.end(); it++) {

			switch( TYPEOF(*it) ) {

			case VECSXP: {
				Rcpp::List tmp = Rcpp::as< Rcpp::List >( *it );
				if(Rf_isNull(tmp.attr("class"))) {
					fetch_geometries(tmp, res, sfg_counter);
				} else {
					res[sfg_counter] = tmp;
					sfg_counter++;
				}
				break;
			}
			case REALSXP: {
				Rcpp::NumericVector tmp = Rcpp::as< Rcpp::NumericVector >( *it );
				if(Rf_isNull(tmp.attr("class"))) {
					Rcpp::stop("Geometry could not be determined");
				} else {
					res[sfg_counter] = tmp;
					sfg_counter++;
				}
				break;
			}
			case INTSXP: {
				Rcpp::IntegerVector tmp = Rcpp::as< Rcpp::IntegerVector >( *it );
				if(Rf_isNull( tmp.attr( "class" ) ) ){
					Rcpp::stop("Geometry could not be determined");
				} else {
					res[sfg_counter] = tmp;
					sfg_counter++;
				}
				break;
			}
			case STRSXP: {
				Rcpp::StringVector tmp = Rcpp::as< Rcpp::StringVector >( *it );
				if(Rf_isNull( tmp.attr( "class" ) ) ) {
					Rcpp::stop("Geometry could not be determined");
				} else {
					res[sfg_counter] = tmp;
					sfg_counter++;
				}
				break;
			}
			default: {
				res[0] = create_null_sfc();
				//Rcpp::stop("Geometry could not be determined");
			}
			}
		}
	}

} // namespace utils
} // namespace sfc
} // namespace geojsonsf

#endif
