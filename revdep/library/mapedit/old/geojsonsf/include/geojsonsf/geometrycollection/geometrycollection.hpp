// #ifndef GEOJSONSF_GEOMETRYCOLLECTION_H
// #define GEOJSONSF_GEOMETRYCOLLECTION_H
//
// #include <Rcpp.h>
//
// #include "sfheaders/utils/utils.hpp"
//
// namespace geojsonsf {
// namespace geometrycollection {
//
// // 	inline void gc_type(
// // 			Rcpp::List& sfg,
// // 			std::string& gc_geom_type,
// // 			bool& isnull,
// // 			Rcpp::CharacterVector& cls
// //   ) {
// //
// // 		for (Rcpp::List::iterator it = sfg.begin(); it != sfg.end(); it++) {
// //
// // 			switch( TYPEOF( *it ) ) {
// // 			case VECSXP: {
// // 				Rcpp::List tmp = Rcpp::as< Rcpp::List >(*it);
// // 				if (!Rf_isNull(tmp.attr("class"))) {
// //
// // 					cls = tmp.attr("class");
// // 					// TODO: error handle (there should aways be 3 elements as we're workgin wtih sfg objects)
// // 					gc_geom_type = cls[1];
// //
// // 					SEXP tst = *it;
// // 					isnull = sfheaders::utils::is_null_geometry( tst, gc_geom_type );
// // 				} else {
// // 					gc_type(tmp, gc_geom_type, isnull, cls);
// // 				}
// // 				break;
// // 			}
// // 			case REALSXP: {
// // 				Rcpp::NumericVector tmp = Rcpp::as< Rcpp::NumericVector >( *it );
// // 				if (!Rf_isNull(tmp.attr("class"))) {
// //
// // 					cls = tmp.attr("class");
// // 					gc_geom_type = cls[1];
// //
// // 					SEXP tst = *it;
// // 					isnull = sfheaders::utils::is_null_geometry( tst, gc_geom_type );
// // 				}
// // 				break;
// // 			}
// // 			default: {
// // 				Rcpp::stop("Coordinates could not be found");
// // 			}
// // 			}
// // 		}
// // 	}
//
// } // namespace geometrycollection
// } // namespace geojsonsf
//
// #endif
