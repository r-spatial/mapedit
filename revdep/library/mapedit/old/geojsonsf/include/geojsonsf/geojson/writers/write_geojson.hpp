#ifndef GEOJSONSF_GEOJSON_WRITERS_WRITE_GEOJSON_H
#define GEOJSONSF_GEOJSON_WRITERS_WRITE_GEOJSON_H

#include <Rcpp.h>
#include "sfheaders/utils/utils.hpp"
#include "geojsonsf/geojson/writers/writers.hpp"
//#include "geojsonsf/geometrycollection/geometrycollection.hpp"


namespace geojsonsf {
namespace write_geojson {

	inline void gc_type(
			Rcpp::List& sfg,
			std::string& gc_geom_type,
			bool& isnull,
			Rcpp::CharacterVector& cls
	) {

		for (Rcpp::List::iterator it = sfg.begin(); it != sfg.end(); it++) {

			switch( TYPEOF( *it ) ) {
			case VECSXP: {
				Rcpp::List tmp = Rcpp::as< Rcpp::List >(*it);
				if (!Rf_isNull(tmp.attr("class"))) {

					cls = tmp.attr("class");
					// TODO: error handle (there should aways be 3 elements as we're workgin wtih sfg objects)
					gc_geom_type = cls[1];

					SEXP tst = *it;
					isnull = sfheaders::utils::is_null_geometry( tst, gc_geom_type );
				} else {
					gc_type(tmp, gc_geom_type, isnull, cls);
				}
				break;
			}
			case REALSXP: {
				Rcpp::NumericVector tmp = Rcpp::as< Rcpp::NumericVector >( *it );
				if (!Rf_isNull(tmp.attr("class"))) {

					cls = tmp.attr("class");
					gc_geom_type = cls[1];

					SEXP tst = *it;
					isnull = sfheaders::utils::is_null_geometry( tst, gc_geom_type );
				}
				break;
			}
			default: {
				Rcpp::stop("Coordinates could not be found");
			}
			}
		}
	}

	template< typename Writer >
	inline void write_geojson(
			Writer& writer,
			SEXP sfg,
			std::string& geom_type,
			Rcpp::CharacterVector& cls, int& digits
		) {

		if (geom_type == "POINT") {
			geojsonsf::writers::points_to_geojson( writer, sfg, digits );

		} else if (geom_type == "MULTIPOINT") {
			geojsonsf::writers::linestring_to_geojson( writer, sfg, digits );

		} else if (geom_type == "LINESTRING") {
			geojsonsf::writers::linestring_to_geojson( writer, sfg, digits );

		} else if (geom_type == "MULTILINESTRING") {
			Rcpp::List multiline = Rcpp::as< Rcpp::List >( sfg );
			geojsonsf::writers::polygon_to_geojson( writer, multiline, digits );

		} else if (geom_type == "POLYGON") {
			Rcpp::List polygon = Rcpp::as< Rcpp::List >(sfg);
			geojsonsf::writers::polygon_to_geojson( writer, polygon, digits );

		} else if (geom_type == "MULTIPOLYGON") {
			Rcpp::List multipolygon = Rcpp::as< Rcpp::List >( sfg );
			geojsonsf::writers::multi_polygon_to_geojson( writer, multipolygon, digits );

		} else if (geom_type == "GEOMETRYCOLLECTION") {
			Rcpp::List gc = Rcpp::as< Rcpp::List >( sfg );
			Rcpp::List sfgi(1);
			R_xlen_t i;
			R_xlen_t gc_n = gc.size();
			for ( i = 0; i < gc_n; ++i) {
				sfgi[0] = gc[i];
				std::string gc_geom_type;
				bool isnull = false;
				gc_type( sfgi, gc_geom_type, isnull, cls );

				if( !isnull ) {
					SEXP sfg_gc = gc[i];
					geojsonsf::writers::begin_geojson_geometry(writer, gc_geom_type);
					write_geojson( writer, sfg_gc, gc_geom_type, cls, digits );
					geojsonsf::writers::end_geojson_geometry(writer, gc_geom_type);
				}
			}
		}
	}


	/*
	 * used for down-casting MULTI objects
	 */
	template< typename Writer >
	inline void write_geojson(
			Writer& writer,
			SEXP sfg,
			std::string& geom_type,
			Rcpp::CharacterVector& cls,
			R_xlen_t geometry_index,
			int& digits
		) {

		if (geom_type == "POINT") {
			geojsonsf::writers::points_to_geojson( writer, sfg, digits );

		} else if (geom_type == "MULTIPOINT") {
			Rcpp::NumericMatrix mls = Rcpp::as< Rcpp::NumericMatrix >( sfg );
			Rcpp::NumericVector pts = mls(geometry_index, Rcpp::_ );
			geojsonsf::writers::points_to_geojson( writer, pts, digits );

		} else if (geom_type == "LINESTRING") {
			geojsonsf::writers::linestring_to_geojson( writer, sfg, digits );

		} else if (geom_type == "MULTILINESTRING") {
			Rcpp::List multiline = Rcpp::as< Rcpp::List >( sfg );
			SEXP ml = multiline[ geometry_index ];
			geojsonsf::writers::linestring_to_geojson( writer, ml, digits );

		} else if (geom_type == "POLYGON") {
			Rcpp::List polygon = Rcpp::as< Rcpp::List >(sfg);
			geojsonsf::writers::polygon_to_geojson( writer, polygon, digits );

		} else if (geom_type == "MULTIPOLYGON") {
			Rcpp::List multipolygon = Rcpp::as< Rcpp::List >( sfg );
			Rcpp::List mlp = multipolygon[ geometry_index ];
			geojsonsf::writers::polygon_to_geojson( writer, mlp, digits );

		} else if (geom_type == "GEOMETRYCOLLECTION") {
			Rcpp::List gc = Rcpp::as< Rcpp::List >( sfg );
			Rcpp::List sfgi(1);
			R_xlen_t i;
			R_xlen_t gc_n = gc.size();
			for ( i = 0; i < gc_n; ++i ) {
				sfgi[0] = gc[i];

				std::string gc_geom_type;
				bool isnull = false;
				gc_type( sfgi, gc_geom_type, isnull, cls );

				if( !isnull ) {
					SEXP sfg_gc = gc[i];
					geojsonsf::writers::begin_geojson_geometry(writer, gc_geom_type);
					write_geojson( writer, sfg_gc, gc_geom_type, cls, digits );
					geojsonsf::writers::end_geojson_geometry(writer, gc_geom_type);
				}

			}
		}
	}

} // namespace geojsonsf
} // namespace write_geojson

#endif
