#ifndef GEOJSONSF_GEOJSON_H
#define GEOJSONSF_GEOJSON_H

#include "geojsonsf/geojsonsf.h"
#include "geojsonsf/geojson/writers/writers.hpp"
#include "geojsonsf/write_geojson.hpp"

#include "geometries/utils/null/null.hpp"
#include "geometries/utils/sexp/sexp.hpp"

#include "sfheaders/sfheaders.hpp"
#include "sfheaders/utils/utils.hpp"

//#include "sfheaders/utils/sexp/sexp.hpp"

namespace geojsonsf {
namespace write_geometry {

	inline void cls_check( Rcpp::CharacterVector& cls ) {
		if (cls.size() != 3 ) {
			Rcpp::stop("unknown sf class");
		}
	}

  /*
   * Write Geometry
   *
   * The input will be a data.frame, so every row should be a POINT (not nested lists), so
   * no requriement to keep track of sfg indeces
   */
	template< typename Writer >
	inline void write_geometry(
			Writer& writer,
			SEXP sfg,
			Rcpp::CharacterVector& cls,
			int digits
		) {

		std::string geom_type;
		geom_type = cls[1];

		int sfglength = geometries::utils::sexp_length( sfg );

		if (sfglength == 0) {
			writer.Null();
		} else {

			bool isnull = sfheaders::utils::is_null_geometry( sfg, geom_type );
			if ( isnull ) {
				writer.Null();
			} else {
				geojsonsf::writers::begin_geojson_geometry(writer, geom_type);
				geojsonsf::write_geojson::write_geojson(writer, sfg, geom_type, cls, digits );
				geojsonsf::writers::end_geojson_geometry( writer, geom_type );
			}
		}
	}


	/*
	 * Write geometry
	 *
	 * the standard function for writing GeoJSON geometries
	 */
	template< typename Writer >
	inline void write_geometry(
			Writer& writer,
			Rcpp::List& sfc,
			R_xlen_t sfg_index,
			int digits
		) {

		SEXP sfg = sfc[ sfg_index ];

		std::string geom_type;
		Rcpp::CharacterVector cls = sfheaders::sfc::getSfClass(sfg);
		cls_check( cls );
		geom_type = cls[1];

		// need to keep track of GEOMETRYCOLLECTIONs so we can correctly close them
		bool isGeometryCollection = (geom_type == "GEOMETRYCOLLECTION") ? true : false;

		int sfglength = geometries::utils::sexp_length( sfg );

		if (sfglength == 0) {
			writer.Null();
		} else {

			bool isnull = sfheaders::utils::is_null_geometry( sfg, geom_type );
			if ( isnull ) {
				writer.Null();
			} else {
				geojsonsf::writers::begin_geojson_geometry(writer, geom_type);
				geojsonsf::write_geojson::write_geojson(writer, sfg, geom_type, cls, digits );

				geom_type = (isGeometryCollection) ? "GEOMETRYCOLLECTION" : geom_type;
				geojsonsf::writers::end_geojson_geometry( writer, geom_type );
			}
		}
	}

	/*
	 * write geometry
	 * down-casting MULTI* geometries to their simpler form
	 */
	template< typename Writer >
	inline void write_geometry(
			Writer& writer,
			Rcpp::List& sfc,
			R_xlen_t sfg_index,
			R_xlen_t geometry_index,
			std::string& geom_type,
			Rcpp::CharacterVector& cls,
			int digits
		) {

		SEXP sfg = sfc[ sfg_index ];
		std::string downcast_geometry;

		if ( geom_type == "MULTIPOINT") {
			downcast_geometry = "POINT";
		} else if ( geom_type == "MULTILINESTRING" ) {
			downcast_geometry = "LINESTRING";
		} else if ( geom_type == "MULTIPOLYGON" ) {
			downcast_geometry = "POLYGON";
		} else {
			downcast_geometry = geom_type;
		}

		// need to keep track of GEOMETRYCOLLECTIONs so we can correctly close them
		bool isGeometryCollection = (geom_type == "GEOMETRYCOLLECTION") ? true : false;

		int sfglength = geometries::utils::sexp_length( sfg );

		if (sfglength == 0) {
			writer.Null();
		} else {

			bool isnull = sfheaders::utils::is_null_geometry( sfg, geom_type );
			if ( isnull ) {
				writer.Null();
			} else {

				geojsonsf::writers::begin_geojson_geometry( writer, downcast_geometry );
				geojsonsf::write_geojson::write_geojson( writer, sfg, geom_type, cls, geometry_index, digits );

				geom_type = (isGeometryCollection) ? "GEOMETRYCOLLECTION" : geom_type;
				geojsonsf::writers::end_geojson_geometry( writer, downcast_geometry );
			}
		}
	}

} // namespace geojson
} // namespace geojsonsf

#endif
