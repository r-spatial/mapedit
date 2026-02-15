// #ifndef R_SFHEADERS_CAST_LIST_H
// #define R_SFHEADERS_CAST_LIST_H
//
// #include "geometries/utils/lists/unlist.hpp"
//
// namespace sfheaders {
// namespace cast {
//
//   // each input is a list;
//   // inline SEXP point_to_point( SEXP obj ) {
//   //   return obj;
//   // }
//   //
//   // inline SEXP point_to_multipoint( SEXP obj ) {
//   //   Rcpp::List res(1);
//   //   res[0] = obj;
//   //   return res;
//   // }
//   //
//   // inline SEXP point_to_linestring( SEXP obj ) {
//   //   return point_to_multipoint( obj );
//   // }
//   //
//   // inline SEXP point_to_multilinestring( SEXP obj ) {
//   //   Rcpp::List res(1);
//   //   res[0] = point_to_multipoint( obj );
//   //   return res;
//   // }
//   //
//   // inline SEXP point_to_polygon( SEXP obj ) {
//   //   return point_to_multilinestring( obj );
//   // }
//   //
//   // inline SEXP point_to_multipolygon( SEXP obj ) {
//   //   Rcpp::List res(1);
//   //   res[0] = point_to_multilinestring( obj );
//   //   return res;
//   // }
//   //
//   //
//   // inline SEXP multipoint_to_point( Rcpp::List lst ) {
//   //   return geometries::utils::unlist_list( lst );
//   // }
//   //
//   // inline SEXP multipoint_to_multipoint( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP multipoint_to_linestring( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP multipoint_to_multilinestring( Rcpp::List lst ) {
//   //   Rcpp::List ml(1);
//   //   ml[0] = lst;
//   //   return ml;
//   // }
//   //
//   // inline SEXP multipoint_to_polygon( Rcpp::List lst ) {
//   //   return multipoint_to_multilinestring( lst );
//   // }
//   //
//   // inline SEXP multipoint_to_multipolygon( Rcpp::List lst ) {
//   //   Rcpp::List mpl(1);
//   //   mpl[0] = lst;
//   //   Rcpp::List res(1);
//   //   res[0] = mpl;
//   //   return res;
//   // }
//   //
//   // inline SEXP linestring_to_point( Rcpp::List lst ) {
//   //   return geometries::utils::unlist_list( lst );
//   // }
//   //
//   // inline SEXP linestring_to_multipoint( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP linestring_to_linestring( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP linestring_to_multilinestring( Rcpp::List lst ) {
//   //   return multipoint_to_multilinestring( lst );
//   // }
//   //
//   // inline SEXP linestring_to_polygon( Rcpp::List lst ) {
//   //   return linestring_to_multilinestring(lst);
//   // }
//   //
//   // inline SEXP linestring_to_multipolygon( Rcpp::List lst ) {
//   //   return multipoint_to_multipolygon( lst );
//   // }
//   //
//   // inline SEXP multilinestring_to_point( Rcpp::List lst ) {
//   //   return geometries::utils::unlist_list(lst);
//   // }
//   //
//   // inline SEXP multilinestring_to_multipoint( Rcpp::List lst ) {
//   //   if( lst.size() != 1 ) {
//   //     Rcpp::stop("sfheaders - list column must have one element");
//   //   }
//   //
//   //   // TODO:
//   //   // - need to remove one level of nesting.
//   //   // - that's it, right?
//   //
//   //   Rcpp::List inner_list = lst[ 0 ];
//   //
//   //   //eturn inner_list;
//   //
//   //   R_xlen_t n = inner_list.size();
//   //   R_xlen_t i;
//   //   Rcpp::List res( n );
//   //   for( i = 0; i < n; ++i ) {
//   //     SEXP obj = inner_list[ i ];
//   //     res[ i ] = obj;
//   //   }
//   //   return res;
//   // }
//   //
//   // inline SEXP multilinestring_to_linestring( Rcpp::List lst ) {
//   //   return multilinestring_to_multipoint( lst );
//   // }
//   //
//   // inline SEXP multilinestring_to_multilinestring( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP multilinestring_to_polygon( Rcpp::List lst ) {
//   //   return lst;
//   // }
//   //
//   // inline SEXP multilinestring_to_multipolygon( Rcpp::List lst ) {
//   //   Rcpp::List mpl(1);
//   //   mpl[0] = lst;
//   //   return mpl;
//   // }
//   //
//   // inline SEXP polygon_to_point( Rcpp::List lst ) {
//   //   return multilinestring_to_point( lst );
//   // }
//   //
//   // inline SEXP polygon_to_multipoint( Rcpp::List lst ) {
//   //   return multilinestring_to_multipoint( lst );
//   // }
//   //
//   // inline SEXP polygon_to_linestring( Rcpp::List lst ) {
//   //   return multilinestring_to_linestring( lst );
//   // }
//   //
//   // inline SEXP polygon_to_multilinestring( Rcpp::List lst ) {
//   //   return multilinestring_to_multilinestring( lst );
//   // }
//   //
//   // inline SEXP polygon_to_polygon( Rcpp::List lst ) {
//   //   return multilinestring_to_polygon( lst );
//   // }
//   //
//   // inline SEXP polygon_to_multipolygon( Rcpp::List lst ) {
//   //   return multilinestring_to_multipolygon( lst );
//   // }
//   //
//   // inline SEXP multipolygon_to_point( Rcpp::List lst ) {
//   //   return geometries::utils::unlist_list( lst );
//   // }
//   //
//   // inline SEXP multipolygon_to_multipoint( Rcpp::List lst ) {
//   //   if( lst.size() != 1 ) {
//   //     Rcpp::stop("sfheaders - list column must have one element");
//   //   }
//   //
//   //   Rcpp::List inner_list = lst[ 0 ];
//   //
//   //   R_xlen_t n = inner_list.size();
//   //   R_xlen_t i;
//   //   Rcpp::List res( n );
//   //   for( i = 0; i < n; ++i ) {
//   //     Rcpp::List obj = inner_list[ i ];
//   //     res[ i ] = multilinestring_to_multipoint( obj );
//   //   }
//   //   return res;
//   // }
//
//   // inline SEXP multipolygon_to_linestring( Rcpp::List lst ) {
//   //   //return multipolygon_to_multipoint( lst );
//   //
//   //   // will return more than 1 list
//   //   R_xlen_t n = lst.size();
//   //   Rcpp::List lines( n );
//   //   R_xlen_t n_linestrings = 0; // will update each iteration
//   //   R_xlen_t i, j;
//   //   for( i = 0; i < n; ++i ) {
//   //     Rcpp::List inner_lst = lst[ i ];
//   //     n_linestrings = n_linestrings + inner_lst.size();
//   //     lines[ i ] = inner_lst;
//   //   }
//   //
//   //   // unpack
//   //   Rcpp::List res( n_linestrings );
//   //   R_xlen_t counter = 0;
//   //   for( i = 0; i < n; ++i ) {
//   //     Rcpp::List sfg = lines[ i ];
//   //     for( j = 0; j < sfg.size(); ++j ) {
//   //       res[ counter ] = sfg[ j ];
//   //       ++counter;
//   //     }
//   //   }
//   //   return res;
//   // }
//
//   // polygon to linestring
//   // polygon to multipoint
//   // multipolygon to multilinestring
//   // multipolygon to polygon
//   // inline SEXP remove_one( Rcpp::List& lst ) {
//   //   R_xlen_t n = lst.size();
//   //
//   //   R_xlen_t i;
//   //   Rcpp::List sfgs( n );
//   //
//   //   for( i = 0; i < n; ++i ) {
//   //     SEXP x = lst[ i ];
//   //     sfgs[ i ] = x;
//   //   }
//   //   return sfgs;
//   // }
//
//   // multipolygon to linestring
//   // multipolygon to multipoint
//
//
//   inline SEXP remove_one( Rcpp::List& lst ) {
//     R_xlen_t n = lst.size();
//     Rcpp::List lines( n );
//     R_xlen_t n_linestrings = 0; // will update each iteration
//     R_xlen_t i, j;
//     for( i = 0; i < n; ++i ) {
//       Rcpp::List inner_lst = lst[ i ];
//       n_linestrings = n_linestrings + inner_lst.size();
//       lines[ i ] = inner_lst;
//     }
//
//     // unpack
//     Rcpp::List res( n_linestrings );
//     R_xlen_t counter = 0;
//     for( i = 0; i < n; ++i ) {
//       Rcpp::List sfg = lines[ i ];
//       for( j = 0; j < sfg.size(); ++j ) {
//         res[ counter ] = sfg[ j ];
//         ++counter;
//       }
//     }
//     return res;
//   }
//
//   // inline SEXP multipolygon_to_multilinestring( Rcpp::List lst ) {
//   //   return multilinestring_to_multipoint( lst );
//   // }
//   //
//   // inline SEXP multipolygon_to_polygon( Rcpp::List lst ) {
//   //   return multilinestring_to_multipoint( lst );
//   // }
//   //
//   // inline SEXP multipolygon_to_multipolygon( Rcpp::List lst ) {
//   //   return lst;
//   // }
//
// } // cast
// } // sfheaders
//
//
// #endif
