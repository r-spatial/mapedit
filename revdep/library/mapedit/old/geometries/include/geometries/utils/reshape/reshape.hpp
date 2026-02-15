// #ifndef R_GEOMETRIES_UTILS_RESHAPE_H
// #define R_GEOMETRIES_UTILS_RESHAPE_H
//
// namespace geometries {
// namespace utils {
//
//   /*
//    * longify
//    *
//    * Assumes the coordinates are in a single Row
//    *
//    * @param coordinate_columns List, should have at least 2 entries
//    * and each entry will specify the colunms to use as coordinates
//    * (and should all be the same length)
//    */
//   template < int RTYPE >
//   inline SEXP longify(
//       Rcpp::Matrix< RTYPE > x,
//       Rcpp::List coordinate_columns
//   ) {
//
//     // TODO:
//     // should this re-shape into a long matrix
//     // or return a list of new matrices
//
//
//     R_xlen_t n = coordinate_columns.size();
//     R_xlen_t i;
//     Rcpp::List res( n );
//
//     for( i = 0; i < n; ++i ) {
//       SEXP cols = coordinate_columns[ i ];
//
//     }
//
//     return res;
//   }
//
// } // utils
// } // sfheaders
//
// #endif
