#ifndef R_SFHEADERS_CAST_SFG_H
#define R_SFHEADERS_CAST_SFG_H

#include "geometries/utils/utils.hpp"

#include "sfheaders/df/sfc.hpp"
#include "sfheaders/sfg/sfg.hpp"

#include <Rcpp.h>

#define M_COLUMN_CAST                9
#define Z_COLUMN_CAST                8
#define Y_COLUMN_CAST                7
#define X_COLUMN_CAST                6
#define VECTOR_ID                    5
#define MATRIX_ID                    4
#define LIST_MATRIX_ID               3
#define LIST_LIST_MATRIX_ID          2
#define SFG_COLUMN_CAST              1
#define SFC_COLUMN_CAST              0

#define MAX_COLUMNS_CAST             10

namespace sfheaders {
namespace cast {

  inline R_xlen_t count_listMatrices( Rcpp::List& lst ) {
    return lst.size();
  }

  inline R_xlen_t count_listListMatrices( Rcpp::List& lst ) {
    // counts all the matrices inside a listList object
    R_xlen_t i, n;
    R_xlen_t res = 0;
    n = lst.size();
    for( i = 0; i < n; ++i ) {
      Rcpp::List inner_lst = lst[ i ];
      res = res + inner_lst.size();
    }
    return res;
  }

  inline R_xlen_t count_new_point_objects(
      SEXP& sfg
  ) {
    R_xlen_t n_coords = 0;
    geometries::coordinates::geometry_dimension( sfg, n_coords );
    return n_coords;
  }


  inline R_xlen_t count_new_multipoint_objects(
      SEXP& sfg,
      std::string& geometry
  ) {
    if( geometry == "POINT" ) {
      return 1;
    } else if ( geometry == "MULTIPOINT") {
      return 1;
    } else if ( geometry == "LINESTRING" ) {
      return 1;
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return lst.size();
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return count_listMatrices( lst );
    } else if ( geometry == "MULTIPOLYGON" ) {
      // count the size of the inner-inner-lists
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return count_listListMatrices( lst );
    } else {
      Rcpp::stop("sfheaders - I can't cast this type of object");  // #nocov
    }
  }

  inline R_xlen_t count_new_linestring_objects(
      SEXP& sfg,
      std::string& geometry
  ) {
    if( geometry == "POINT" ) {
      return 1;
    } else if ( geometry == "MULTIPOINT") {
      return 1;
    } else if ( geometry == "LINESTRING" ) {
      return 1;
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return lst.size();
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return count_listMatrices( lst );
    } else if ( geometry == "MULTIPOLYGON" ) {
      // count the size of the inner-inner-lists
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return count_listListMatrices( lst );
    } else {
      Rcpp::stop("sfheaders - I can't cast this type of object"); // #nocov
    }
  }

  inline R_xlen_t count_new_multilinestring_objects(
      SEXP& sfg,
      std::string& geometry
  ) {
    // going to polygon

    if( geometry == "POINT" ) {
      // going from POINT to POLYGON
      // (does this even make sense?)
      return 1;
    } else if ( geometry == "MULTIPOINT") {
      return 1;
    } else if ( geometry == "LINESTRING" ) {
      return 1;
    } else if ( geometry == "MULTILINESTRING" ) {
      return 1;
    } else if ( geometry == "POLYGON" ) {
      return 1;
    } else if ( geometry == "MULTIPOLYGON" ) {
      // count the size of the inner-lists
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return lst.size();
    } else {
      Rcpp::stop("sfheaders - I can't cast this type of object");  // #nocov
    }
  }

  inline R_xlen_t count_new_polygon_objects(
      SEXP& sfg,
      std::string& geometry
  ) {
    // going to polygon

    if( geometry == "POINT" ) {
      // going from POINT to POLYGON
      // (does this even make sense?)
      return 1;
    } else if ( geometry == "MULTIPOINT") {
      return 1;
    } else if ( geometry == "LINESTRING" ) {
      return 1;
    } else if ( geometry == "MULTILINESTRING" ) {
      return 1;
    } else if ( geometry == "POLYGON" ) {
      return 1;
    } else if ( geometry == "MULTIPOLYGON" ) {
      // count the size of the inner-list
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return lst.size();
    } else {
      Rcpp::stop("sfheaders - I can't cast this type of object");  // #nocov
    }
  }

  inline R_xlen_t count_new_multipolygon_objects( SEXP& sfg, std::string& geometry ) {
    // going to multipolygon
    return 1;
  }


  inline R_xlen_t count_new_objects( SEXP& sfg, std::string cast_to ) {

    Rcpp::CharacterVector cls = sfheaders::utils::getSfgClass( sfg );
    std::string geometry;
    geometry = cls[1];

    if( cast_to == "POINT" ) {
      return count_new_point_objects( sfg );
    } else if ( cast_to == "MULTIPOINT" ) {
      return count_new_multipoint_objects( sfg, geometry );
    } else if ( cast_to == "LINESTRING" ) {
      return count_new_linestring_objects( sfg, geometry );
    } else if ( cast_to == "MULTILINESTRING" ) {
      return count_new_multilinestring_objects( sfg, geometry );
    } else if ( cast_to == "POLYGON" ) {
      return count_new_polygon_objects( sfg, geometry );
    } else if ( cast_to == "MULTIPOLYGON" ) {
      return count_new_multipolygon_objects( sfg, geometry );
    } else {
      Rcpp::stop("sfheaders - I don't know the type of object you're trying to cast to");  // #nocov
    }

  }

  inline SEXP point_to_multilinestring( Rcpp::NumericVector& nv, std::string xyzm ) {
    return sfheaders::sfg::sfg_multilinestring( nv, xyzm );
  }

  inline SEXP point_to_linestring( Rcpp::NumericVector& nv, std::string xyzm ) {
    return sfheaders::sfg::sfg_linestring( nv, xyzm );
  }

  inline SEXP point_to_multipoint( Rcpp::NumericVector& nv, std::string xyzm ) {
    return sfheaders::sfg::sfg_multipoint( nv, xyzm );
  }

  inline SEXP multipoint_to_point( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    return sfheaders::sfg::sfg_points( nm, xyzm );
  }

  inline SEXP multipoint_to_linestring( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    Rcpp::NumericMatrix nm2 = Rcpp::clone( nm );
    return sfheaders::sfg::sfg_linestring( nm2, xyzm );
  }

  inline SEXP multipoint_to_multilinestring( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    Rcpp::List mls(1);
    mls[0] = nm;
    return sfheaders::sfg::sfg_multilinestring( mls, xyzm );
  }

  inline SEXP multipoint_to_polygon( Rcpp::NumericMatrix& nm, std::string xyzm, bool close = true ) {
    Rcpp::List mpl(1);
    mpl[0] = nm;
    return sfheaders::sfg::sfg_polygon( mpl, xyzm, close );
  }

  inline SEXP multipoint_to_multipolygon( Rcpp::NumericMatrix& nm, std::string xyzm, bool close = true ) {
    Rcpp::List mpl(1);
    mpl[0] = nm;
    Rcpp::List mpl2(1);
    mpl2[0] = mpl;
    return sfheaders::sfg::sfg_multipolygon( mpl2, xyzm, close );
  }

  inline SEXP linestring_to_point( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    return sfheaders::sfg::sfg_points( nm, xyzm );
  }

  inline SEXP linestring_to_multipoint( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    Rcpp::NumericMatrix nm2 = Rcpp::clone( nm );
    return sfheaders::sfg::sfg_multipoint( nm2, xyzm );
  }

  inline SEXP linestring_to_multilinestring( Rcpp::NumericMatrix& nm, std::string xyzm ) {
    Rcpp::List mls(1);
    mls[0] = nm;
    return sfheaders::sfg::sfg_multilinestring( mls, xyzm );
  }

  inline SEXP linestring_to_polygon( Rcpp::NumericMatrix& nm, std::string xyzm, bool close = true ) {
    Rcpp::List mpl(1);
    mpl[0] = nm;
    return sfheaders::sfg::sfg_polygon( mpl, xyzm, close );
  }

  inline SEXP linestring_to_multipolygon( Rcpp::NumericMatrix& nm, std::string xyzm, bool close = true ) {
    Rcpp::List mpl(1);
    mpl[0] = nm;
    Rcpp::List mpl2(1);
    mpl2[0] = mpl;
    return sfheaders::sfg::sfg_multipolygon( mpl2, xyzm, close );
  }

  inline SEXP multilinestring_to_point( Rcpp::List& lst, std::string xyzm ) {

    R_xlen_t n_points = 0;
    R_xlen_t i, j;
    R_xlen_t n_linestrings = lst.size();
    Rcpp::List points( n_linestrings );
    for( i = 0; i < n_linestrings; ++i ) {
      Rcpp::NumericMatrix nm = lst[ i ];
      n_points = n_points + nm.nrow();
      points[ i ] = sfheaders::sfg::sfg_points( nm, xyzm );
    }

    // unpack
    Rcpp::List res( n_points );
    R_xlen_t counter = 0;

    for( i = 0; i < n_linestrings; ++i ) {
      Rcpp::List sfg_points = points[ i ];

      for( j = 0; j < sfg_points.size(); ++j ) {
        res[ counter ] = sfg_points[ j ];
        ++counter;
      }
    }
    return res;
  }

  inline SEXP multilinestring_to_multipoint( Rcpp::List& lst, std::string xyzm ) {
    return sfheaders::sfg::sfg_multipoints( lst, xyzm );
  }

  inline SEXP multilinestring_to_linestring( Rcpp::List& lst, std::string xyzm ) {
    Rcpp::List l = sfheaders::sfg::sfg_linestrings( lst, xyzm );
    return l;
  }

  inline SEXP multilinestring_to_multilinestring( SEXP& obj, std::string xyzm ) {
    return sfheaders::sfg::sfg_multilinestring( obj, xyzm );
  }

  inline SEXP multilinestring_to_polygon( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    //Rcpp::List lst2(1);
    //lst2[0] = lst[0];   // make a new object so it's not updated by-reference
    return sfheaders::sfg::sfg_polygon( lst2, xyzm, close );
  }

  inline SEXP multilinestring_to_multipolygon( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    Rcpp::List mpl(1);
    mpl[0] = lst2;
    return sfheaders::sfg::sfg_multipolygon( mpl, xyzm, close );
  }

  inline SEXP polygon_to_point( Rcpp::List& lst, std::string xyzm ) {
    R_xlen_t n_points = 0;
    R_xlen_t i, j;
    R_xlen_t n_linestrings = lst.size();
    Rcpp::List points( n_linestrings );
    for( i = 0; i < n_linestrings; ++i ) {
      Rcpp::NumericMatrix nm = lst[ i ];
      n_points = n_points + nm.nrow();
      points[ i ] = sfheaders::sfg::sfg_points( nm, xyzm );
    }

    // unpack
    Rcpp::List res( n_points );
    R_xlen_t counter = 0;

    for( i = 0; i < n_linestrings; ++i ) {
      Rcpp::List sfg_points = points[ i ];

      for( j = 0; j < sfg_points.size(); ++j ) {
        res[ counter ] = sfg_points[ j ];
        ++counter;
      }
    }
    return res;
  }

  inline SEXP polygon_to_multipoint( Rcpp::List& lst, std::string xyzm ) {
    return sfheaders::sfg::sfg_multipoints( lst, xyzm );
  }

  inline SEXP polygon_to_linestring( Rcpp::List& lst, std::string xyzm ) {
    return sfheaders::sfg::sfg_linestrings( lst, xyzm );
  }

  inline SEXP polygon_to_multilinestring( Rcpp::List& lst, std::string xyzm ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    return sfheaders::sfg::sfg_multilinestring( lst2, xyzm );
  }

  inline SEXP polygon_to_multipolygon( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    // //lst.attr("class") = R_NilValue; // this will update the input by-reference
    // // not doing it will leave the sfg_POLYGON class on the inner-polygons of the MULTIPOLYGON
    // // not sure if this is an issue or note.
    Rcpp::List mpl(1);
    mpl[0] = lst2;
    return sfheaders::sfg::sfg_multipolygon( mpl, xyzm, close );
  }

  // any function which down-casts needs to use the pluralised version of the sfg_() code

  inline SEXP multipolygon_to_point( Rcpp::List& lst, std::string xyzm ) {
    // will return more than 1 list
    R_xlen_t n_polygons = lst.size();
    Rcpp::List lines( n_polygons );
    R_xlen_t n_points = 0;
    R_xlen_t i, j, k;
    for( i = 0; i < n_polygons; ++i ) {
      Rcpp::List polygon = lst[ i ];
      R_xlen_t n_linestrings = polygon.size();
      Rcpp::List points( n_linestrings );
      //ned to go round again to do each row of each matrix!
      //and also record n_coordinates for the inner matrice?
      for( j = 0; j < n_linestrings; ++j ) {
        Rcpp::NumericMatrix nm = polygon[ j ];
        n_points = n_points + nm.nrow();
        points[ j ] = sfheaders::sfg::sfg_points( nm, xyzm );
      }
      lines[ i ] = points;
    }

    // unpack
    Rcpp::List res( n_points );
    R_xlen_t counter = 0;

    // if( lines.size() != n_polygons ) {
    //   Rcpp::stop("here 1");
    // }

    for( i = 0; i < n_polygons; ++i ) {

      Rcpp::List sfg_lines = lines[ i ];

      for( j = 0; j < sfg_lines.size(); ++j ) {
        Rcpp::List sfg_points = sfg_lines[ j ];
        for( k = 0; k < sfg_points.size(); ++k ) {
          res[ counter ] = sfg_points[ k ];
          ++counter;
        }

      }
    }
    return res;
  }

  inline SEXP multipolygon_to_multipoint( Rcpp::List& lst, std::string xyzm ) {
    // will return more than 1 list
    R_xlen_t n = lst.size();
    Rcpp::List lines( n );
    R_xlen_t n_linestrings = 0; // will update each iteration
    R_xlen_t i, j;
    for( i = 0; i < n; ++i ) {
      Rcpp::List inner_lst = lst[ i ];
      n_linestrings = n_linestrings + inner_lst.size();
      lines[ i ] = sfheaders::sfg::sfg_multipoints( inner_lst, xyzm );
    }

    // unpack
    Rcpp::List res( n_linestrings );
    R_xlen_t counter = 0;
    for( i = 0; i < n; ++i ) {
      Rcpp::List sfg = lines[ i ];
      for( j = 0; j < sfg.size(); ++j ) {
        res[ counter ] = sfg[ j ];
        ++counter;
      }
    }
    return res;
  }

  inline SEXP multipolygon_to_linestring( Rcpp::List& lst, std::string xyzm ) {

    // will return more than 1 list
    R_xlen_t n = lst.size();
    Rcpp::List lines( n );
    R_xlen_t n_linestrings = 0; // will update each iteration
    R_xlen_t i, j;
    for( i = 0; i < n; ++i ) {
      Rcpp::List inner_lst = lst[ i ];
      n_linestrings = n_linestrings + inner_lst.size();
      lines[ i ] = sfheaders::sfg::sfg_linestrings( inner_lst, xyzm );
    }

    // unpack
    Rcpp::List res( n_linestrings );
    R_xlen_t counter = 0;
    for( i = 0; i < n; ++i ) {
      Rcpp::List sfg = lines[ i ];
      for( j = 0; j < sfg.size(); ++j ) {
        res[ counter ] = sfg[ j ];
        ++counter;
      }
    }
    return res;
  }


  inline SEXP multipolygon_to_multilinestring( Rcpp::List& lst, std::string xyzm ) {
    // multipolygon is a list
    Rcpp::List lst2 = Rcpp::clone( lst );
    return sfheaders::sfg::sfg_multilinestrings( lst2, xyzm );
  }

  inline SEXP multipolygon_to_polygon( Rcpp::List& lst, std::string xyzm, bool close = true ) {
    Rcpp::List lst2 = Rcpp::clone( lst );
    return sfheaders::sfg::sfg_polygons( lst2, xyzm, close );
  }

  inline SEXP cast_to_point( SEXP& sfg, std::string& geometry, std::string xyzm ) {

    if( geometry == "POINT") {
      return sfg;
    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return multipoint_to_point( nm, xyzm );
    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return linestring_to_point( nm, xyzm );
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multilinestring_to_point( lst, xyzm );
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return polygon_to_point( lst, xyzm );
    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multipolygon_to_point( lst, xyzm );
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a POINT"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to_multipoint( SEXP& sfg, std::string& geometry, std::string xyzm ) {

    if( geometry == "POINT") {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sfg );
      return point_to_multipoint( nv, xyzm );
    } else if ( geometry == "MULTIPOINT" ) {
      return sfg;
    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return linestring_to_multipoint( nm, xyzm );
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multilinestring_to_multipoint( lst, xyzm );
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return polygon_to_multipoint( lst, xyzm );
    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multipolygon_to_multipoint( lst, xyzm );
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a MULTIPOINT"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to_linestring( SEXP& sfg, std::string& geometry, std::string xyzm ) {

    if( geometry == "POINT") {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sfg );
      return point_to_linestring( nv, xyzm );
    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return multipoint_to_linestring( nm, xyzm );
    } else if ( geometry == "LINESTRING" ) {
      return sfg;
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multilinestring_to_linestring( lst, xyzm );
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return polygon_to_linestring( lst, xyzm );
    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multipolygon_to_linestring( lst, xyzm );
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a LINESTRING"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to_multilinestring( SEXP& sfg, std::string& geometry, std::string xyzm ) {

    if( geometry == "POINT") {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sfg );
      return point_to_multilinestring( nv, xyzm );
    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return multipoint_to_multilinestring( nm, xyzm );
    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return linestring_to_multilinestring( nm, xyzm );
    } else if ( geometry == "MULTILINESTRING" ) {
      return sfg;
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return polygon_to_multilinestring( lst, xyzm );
    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multipolygon_to_multilinestring( lst, xyzm );
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a POLYGON"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to_polygon( SEXP& sfg, std::string& geometry, std::string xyzm, bool close = true ) {

    if( geometry == "POINT") {
      Rcpp::stop("sfheaders - can't cast from POINT to POLYGON");
    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return multipoint_to_polygon( nm, xyzm, close );
    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return linestring_to_polygon( nm, xyzm, close );
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multilinestring_to_polygon( lst, xyzm, close );
    } else if ( geometry == "POLYGON" ) {
      return sfg;
    } else if ( geometry == "MULTIPOLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multipolygon_to_polygon( lst, xyzm, close );
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a POLYGON"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to_multipolygon( SEXP& sfg, std::string& geometry, std::string xyzm, bool close = true ) {

    if( geometry == "POINT") {
      Rcpp::stop("sfheaders - can't cast from POINT to MULTIPOLYGON");
    } else if ( geometry == "MULTIPOINT" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return multipoint_to_multipolygon( nm, xyzm, close );
    } else if ( geometry == "LINESTRING" ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
      return linestring_to_multipolygon( nm, xyzm, close );
    } else if ( geometry == "MULTILINESTRING" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return multilinestring_to_multipolygon( lst, xyzm, close );
    } else if ( geometry == "POLYGON" ) {
      Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
      return polygon_to_multipolygon( lst, xyzm, close );
    } else if ( geometry == "MULTIPOLYGON" ) {
      return sfg;
    } else {
      Rcpp::stop("sfheaders - I don't know how to convert this objet to a MULTIPOLYGON"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }

  inline SEXP cast_to( SEXP& sfg, std::string& cast_from, std::string& cast_to, std::string xyzm, bool close = true ) {

    if( cast_to == "POINT" ) {
      return cast_to_point( sfg, cast_from, xyzm );
    } else if ( cast_to == "MULTIPOINT" ) {
      return cast_to_multipoint( sfg, cast_from, xyzm );
    } else if ( cast_to == "LINESTRING" ) {
      return cast_to_linestring( sfg, cast_from, xyzm );
    } else if ( cast_to == "MULTILINESTRING" ) {
      return cast_to_multilinestring( sfg, cast_from, xyzm );
    } else if ( cast_to == "POLYGON" ) {
      return cast_to_polygon( sfg, cast_from, xyzm, close );
    } else if ( cast_to == "MULTIPOLYGON" ) {
      return cast_to_multipolygon( sfg, cast_from, xyzm, close);
    } else {
      Rcpp::stop("sfheaders - I don't the type of object you're trying to cast to"); // #nocov
    }
    return Rcpp::List(); // #nocov
  }



  // const Rcpp::CharacterVector column_names = {
  //   "sfc_id", "sfg_id", "list_list_matrix_id", "list_matrix_id", "matrix_id", "vector_id",
  //   "x","y","z","m"
  // };
  //
  // inline Rcpp::CharacterVector make_names( Rcpp::CharacterVector& cls ) {
  //
  //   std::string dim;
  //   std::string geometry;
  //
  //   Rcpp::LogicalVector columns( column_names.length() );
  //
  //   dim = cls[0];
  //   geometry = cls[1];
  //
  //   if ( dim == "XYZM" ) {
  //     columns[ Z_COLUMN_CAST ] = true;
  //     columns[ M_COLUMN_CAST ] = true;
  //   } else if ( dim == "XYZ" ) {
  //     columns[ Z_COLUMN_CAST ] = true;
  //   } else if ( dim == "XYM" ) {
  //     columns[ M_COLUMN_CAST ] = true;  // #nocov
  //   }
  //
  //   columns[ X_COLUMN_CAST ] = true;
  //   columns[ Y_COLUMN_CAST ] = true;
  //
  //   if( geometry == "POINT" ) {
  //   } else if ( geometry == "MULTIPOINT" ) {
  //
  //   } else if ( geometry == "LINESTRING" ) {
  //
  //   } else if ( geometry == "MULTILINESTRING" ) {
  //     columns[ MATRIX_ID ] = true;
  //
  //   } else if ( geometry == "POLYGON" ) {
  //     columns[ MATRIX_ID ] = true;
  //
  //   } else if ( geometry == "MULTIPOLYGON" ) {
  //     columns[ MATRIX_ID ] = true;
  //     columns[ LIST_MATRIX_ID ] = true;
  //   }
  //   return column_names[ columns ];
  // }
  //
  //
  // inline Rcpp::List sfg_to_cast_df( SEXP& sfg ) {
  //
  //   Rcpp::List res;
  //
  //   Rcpp::CharacterVector cls = sfheaders::utils::getSfgClass( sfg );
  //
  //   std::string dim;
  //   std::string geometry;
  //
  //   Rcpp::LogicalVector columns( column_names.length() );
  //
  //   dim = cls[0];
  //   geometry = cls[1];
  //
  //   R_xlen_t sfg_rows = 0;
  //
  //   if( geometry == "POINT" ) {
  //     Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sfg );
  //     res = sfheaders::df::sfg_point_coordinates( nv, sfg_rows );
  //
  //   } else if ( geometry == "MULTIPOINT" ) {
  //     Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
  //     res = sfheaders::df::sfg_multipoint_coordinates( nm, sfg_rows );
  //
  //   } else if ( geometry == "LINESTRING" ) {
  //     Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sfg );
  //     res = sfheaders::df::sfg_linestring_coordinates( nm, sfg_rows );
  //
  //   } else if ( geometry == "MULTILINESTRING" ) {
  //     Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
  //     res = sfheaders::df::sfg_multilinestring_coordinates( lst, sfg_rows );
  //
  //   } else if ( geometry == "POLYGON" ) {
  //     Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
  //     res = sfheaders::df::sfg_polygon_coordinates( lst, sfg_rows );
  //
  //   } else if ( geometry == "MULTIPOLYGON" ) {
  //     Rcpp::List lst = Rcpp::as< Rcpp::List >( sfg );
  //     res = sfheaders::df::sfg_multipolygon_coordinates( lst, sfg_rows );
  //
  //   } else {
  //     Rcpp::stop("sfheaders - unknown geometry type"); // #nocov
  //   }
  //
  //   Rcpp::CharacterVector df_names = make_names( cls );
  //
  //   res.attr("class") = Rcpp::CharacterVector("data.frame");
  //
  //   if( sfg_rows > 0 ) {
  //     Rcpp::IntegerVector rownames = Rcpp::seq( 1, sfg_rows );
  //     res.attr("row.names") = rownames;
  //   } else {
  //     res.attr("row.names") = Rcpp::IntegerVector(0); // #nocov
  //   }
  //
  //   res.attr("names") = df_names;
  //   return res;
  //
  // }

} // cast
} // sfheaders

#endif
