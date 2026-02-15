#ifndef R_SFHEADERS_Z_RANGE_H
#define R_SFHEADERS_Z_RANGE_H

#include <Rcpp.h>
#include "geometries/utils/sexp/sexp.hpp"

namespace sfheaders {
namespace zm {

  inline void attach_z_range_attributes( Rcpp::NumericVector& z_range ) {
    z_range.attr("class") = Rcpp::CharacterVector::create("z_range");
    z_range.attr("names") = Rcpp::CharacterVector::create("zmin","zmax");
  }

  inline Rcpp::NumericVector start_z_range() {
    Rcpp::NumericVector range(2);
    range(0) = range(1) = NA_REAL;
    return range;
  }


  inline void z_range_size_check( Rcpp::IntegerVector& point ) {
    if( point.length() < 3 ) {
      Rcpp::stop("sfheaders - incorrect size of z_range");
    }
  }

  inline void z_range_size_check( Rcpp::NumericVector& point ) {
    if( point.length() < 3 ) {
      Rcpp::stop("sfheaders - incorrect size of z_range");
    }
  }

  inline void z_range_size_check( Rcpp::IntegerMatrix& im ) {
    if( im.ncol() < 3 ) {
      Rcpp::stop("sfheaders - incorrect size of z_range");
    }
  }

  inline void z_range_size_check( Rcpp::NumericMatrix& nm ) {
    if( nm.ncol() < 3 ) {
      Rcpp::stop("sfheaders - incorrect size of z_range");
    }
  }

  inline void z_range_size_check( Rcpp::DataFrame& df ) {
    if( df.ncol() < 3 ) {
      Rcpp::stop("sfheaders - incorrect size of z_range");
    }
  }

  inline void z_range_size_check( SEXP& x ) {
    switch( TYPEOF( x ) ) {
    case INTSXP: {
    if( Rf_isMatrix( x ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
      z_range_size_check( im );
    } else {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
      z_range_size_check( iv );
    }
    break;
    }
    case REALSXP: {
    if( Rf_isMatrix( x ) ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
      z_range_size_check( nm );
    } else {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
      z_range_size_check( nv );
    }
    break;
    }
    case VECSXP: {
    if( Rf_inherits( x, "data.frame" ) ) {
      Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
      z_range_size_check( df );
      break;
    }
    }
    default: {
      Rcpp::stop("sfheaders - invalid size of z_range");
    }
    }
  }


  // template< int RTYPE, typename T >
  // inline void calculate_z_range(
  //     Rcpp::Vector< RTYPE >& z_range,
  //     T& val
  // ) {
  //   T d = static_cast< T >( val );
  //   //calculate_z_range( z_range, d );
  // }


  template< int RTYPE >
  inline void calculate_z_range(
      Rcpp::Vector< RTYPE >& z_range,
      Rcpp::Vector< RTYPE >& point
  ) {
    z_range_size_check( point );

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    T d = point[2];

    if ( std::isnan( d ) ) { return; };

    //xmin, ymin, xmax, ymax
    z_range[0] = std::min( d, z_range[0] );
    z_range[1] = std::max( d, z_range[1] );

    // Rcpp::Rcout << "z_range 13 : " << z_range << std::endl;
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::IntegerVector& point
  ) {
    z_range_size_check( point );

    int i = point[2];

    double d = static_cast< double >( i );

    if ( std::isnan( d ) ) { return; };

    z_range[0] = std::min( d, z_range[0] );
    z_range[1] = std::max( d, z_range[1] );

    // Rcpp::Rcout << "z_range 12 : " << z_range << std::endl;

    //calculate_z_range( z_range, d );
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::IntegerMatrix& im
  ) {

    z_range_size_check( im );

    Rcpp::IntegerVector z = im( Rcpp::_, 2 );

    int zmin = Rcpp::min( z );
    int zmax = Rcpp::max( z );

    double mn = static_cast< double >( zmin );
    double mx = static_cast< double >( zmax );

    z_range[0] = std::isnan( mn ) ? z_range[0] : std::min( mn, z_range[0] );
    z_range[1] = std::isnan( mx ) ? z_range[1] : std::max( mx, z_range[1] );

    // Rcpp::Rcout << "z_range 11 : " << z_range << std::endl;
  }

  template< int RTYPE >
  inline void calculate_z_range(
      Rcpp::Vector< RTYPE >& z_range,
      Rcpp::Matrix< RTYPE >& nm
  ) {

    z_range_size_check( nm );

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    Rcpp::NumericVector z = nm( Rcpp::_, 2 );

    T zmin = Rcpp::min( z );
    T zmax = Rcpp::max( z );

    z_range[0] = std::min( zmin, z_range[0] );
    z_range[1] = std::max( zmax, z_range[1] );

    // Rcpp::Rcout << "z_range 10 : " << z_range << std::endl;

  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::DataFrame& df
  ) {
    // assumes 'x' & 'y' column vectors
    z_range_size_check( df );

    Rcpp::NumericVector z = df[2];

    double zmin = Rcpp::min( z );
    double zmax = Rcpp::max( z );

    z_range[0] = std::min( zmin, z_range[0] );
    z_range[1] = std::max( zmax, z_range[1] );

    // Rcpp::Rcout << "z_range 9 : " << z_range << std::endl;

  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      SEXP& x
  ) {

    switch( TYPEOF( x ) ) {
    case INTSXP: {
      if( Rf_isMatrix( x ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
      calculate_z_range( z_range, im );
    } else {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
      calculate_z_range( z_range, iv );
    }
    break;
    }
    case REALSXP: {
      if( Rf_isMatrix( x ) ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
      calculate_z_range( z_range, nm );
    } else {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
      calculate_z_range( z_range, nv );
    }
    break;
    }
    case VECSXP: {
      if( Rf_inherits( x, "data.frame" ) ) {
      Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
      calculate_z_range( z_range, df );
      break;
    } // else default
    }
    default: {
      Rcpp::stop("sfheaders - can't calculate bounding box for this type");  // #nocov
    }
    }
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::IntegerVector& iv,
      Rcpp::IntegerVector& geometry_cols
  ) {

    if( geometry_cols.length() > 2 ) {
      int idx = geometry_cols[2];

      int z = iv[ idx ];

      double zz = static_cast< double >( z );

      if ( std::isnan( zz ) ) { return; };

      z_range[0] = std::min( zz, z_range[0] );
      z_range[1] = std::max( zz, z_range[1] );

      // Rcpp::Rcout << "z_range 8 : " << z_range << std::endl;
    }
  }

  template< int RTYPE >
  inline void calculate_z_range(
      Rcpp::Vector< RTYPE >& z_range,
      Rcpp::Vector< RTYPE >& vec,
      Rcpp::IntegerVector& geometry_cols
  ) {

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    if( geometry_cols.length() > 2 ) {

      int idx = geometry_cols[2];

      T z = vec[ idx ];

      // T zmin = Rcpp::min( z );
      // T zmax = Rcpp::max( z );

      z_range[0] = std::min( z, z_range[0] );
      z_range[1] = std::max( z, z_range[1] );

      // Rcpp::Rcout << "z_range 7 : " << z_range << std::endl;
    }
  }

  inline void calculate_z_range(
    Rcpp::NumericVector& z_range,
    Rcpp::IntegerMatrix& im,
    Rcpp::IntegerVector& geometry_cols
  ) {
    if( geometry_cols.length() > 2 ) {
      int idx = geometry_cols[2];
      Rcpp::IntegerVector z = im( Rcpp::_, idx );

      int zmin = Rcpp::min( z );
      int zmax = Rcpp::max( z );

      double mn = static_cast< double >( zmin );
      double mx = static_cast< double >( zmax );

      z_range[0] = std::min( mn, z_range[0] );
      z_range[1] = std::max( mx, z_range[1] );

      // Rcpp::Rcout << "z_range 6 : " << z_range << std::endl;
    }
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::IntegerMatrix& im,
      Rcpp::StringVector& geometry_cols
  ) {

    Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( im );

    if( geometry_cols.length() > 2 ) {
      Rcpp::String idx = geometry_cols[2];
      std::string s_idx = idx.get_cstring();

      Rcpp::IntegerVector z = df[ s_idx ];

      int zmin = Rcpp::min( z );
      int zmax = Rcpp::max( z );

      double mn = static_cast< double >( zmin );
      double mx = static_cast< double >( zmax );

      z_range[0] = std::min( mn, z_range[0] );
      z_range[1] = std::max( mx, z_range[1] );

      // Rcpp::Rcout << "z_range 5 : " << z_range << std::endl;
    }
  }

  template< int RTYPE >
  inline void calculate_z_range(
      Rcpp::Vector< RTYPE >& z_range,
      Rcpp::Matrix< RTYPE >& nm,
      Rcpp::IntegerVector& geometry_cols
  ) {
    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    if( geometry_cols.length() > 2 ) {
      int idx = geometry_cols[2];
      Rcpp::Vector< RTYPE > z = nm( Rcpp::_, idx );

      T zmin = Rcpp::min( z );
      T zmax = Rcpp::max( z );

      z_range[0] = std::min( zmin, z_range[0] );
      z_range[1] = std::max( zmax, z_range[1] );

      // Rcpp::Rcout << "z_range 4 : " << z_range << std::endl;
    }
  }

  template< int RTYPE >
  inline void calculate_z_range(
      Rcpp::Vector< RTYPE >& z_range,
      Rcpp::Matrix< RTYPE >& mat,
      Rcpp::StringVector& geometry_cols
  ) {

    Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( mat );
    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    if( geometry_cols.length() > 2 ) {
      Rcpp::String idx = geometry_cols[2];
      std::string s_idx = idx.get_cstring();
      Rcpp::Vector< RTYPE > z = df[ s_idx ];

      T zmin = Rcpp::min( z );
      T zmax = Rcpp::max( z );

      z_range[0] = std::min( zmin, z_range[0] );
      z_range[1] = std::max( zmax, z_range[1] );

      // Rcpp::Rcout << "z_range 3 : " << z_range << std::endl;
    }
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::DataFrame& df,
      Rcpp::IntegerVector& geometry_cols
  ) {
    if( geometry_cols.length() > 2 ) {
      int idx = geometry_cols[2];
      Rcpp::NumericVector z = df[ idx ];

      double zmin = Rcpp::min( z );
      double zmax = Rcpp::max( z );

      z_range[0] = std::min( zmin, z_range[0] );
      z_range[1] = std::max( zmax, z_range[1] );

      // Rcpp::Rcout << "z_range 2 : " << z_range << std::endl;
    }
  }

  inline void calculate_z_range(
      Rcpp::NumericVector& z_range,
      Rcpp::DataFrame& df,
      Rcpp::StringVector& geometry_cols
  ) {

    if( geometry_cols.length() > 2 ) {

      Rcpp::String idx = geometry_cols[2];
      std::string s_idx = idx.get_cstring();
      Rcpp::NumericVector z = df[ s_idx ];

      double zmin = Rcpp::min( z );
      double zmax = Rcpp::max( z );

      z_range[0] = std::min( zmin, z_range[0] );
      z_range[1] = std::max( zmax, z_range[1] );

      // Rcpp::Rcout << "z_range 1 : " << z_range << std::endl;
    }
  }

} // bbox
} // sfheaders


#endif
