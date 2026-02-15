#ifndef R_GEOMETRIES_BBOX_H
#define R_GEOMETRIES_BBOX_H

#include "geometries/utils/utils.hpp"

namespace geometries {
namespace bbox {

  template< int RTYPE >
  inline void bbox_size_check( Rcpp::Vector< RTYPE >& point ) {
    if( point.length() < 2 ) {
      Rcpp::stop("geometries - incorrect size of bounding box");
    }
  }

  template< int RTYPE >
  inline void bbox_size_check( Rcpp::Matrix< RTYPE >& m ) {
    if( m.ncol() < 2 ) {
      Rcpp::stop("geometries - incorrect size of bounding box");
    }
  }

  inline void bbox_size_check( Rcpp::DataFrame& df ) {
    if( df.ncol() < 2 ) {
      Rcpp::stop("geometries - incorrect size of bounding box");
    }
  }

  template< int RTYPE >
  inline void make_bbox(
      Rcpp::Vector< RTYPE >& bbox,
      Rcpp::Vector< RTYPE >& x,
      Rcpp::Vector< RTYPE >& y
  ) {

    typedef typename Rcpp::traits::storage_type< RTYPE >::type T;

    T xmin = Rcpp::min( x );
    T ymin = Rcpp::min( y );
    T xmax = Rcpp::max( x );
    T ymax = Rcpp::max( y );

    bbox[0] = std::min( xmin, bbox[0] );
    bbox[2] = std::max( xmax, bbox[2] );

    bbox[1] = std::min( ymin, bbox[1] );
    bbox[3] = std::max( ymax, bbox[3] );
  }

  // TODO: - template bbox type
  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      double& x,
      double& y
  ) {
    //xmin, ymin, xmax, ymax
    bbox[0] = std::min( x, bbox[0] );
    bbox[2] = std::max( x, bbox[2] );

    bbox[1] = std::min( y, bbox[1] );
    bbox[3] = std::max( y, bbox[3] );
  }

  template< int RTYPE >
  inline void calculate_bbox(
      Rcpp::Vector< RTYPE >& bbox,
      Rcpp::Vector< RTYPE >& point
  ) {

    bbox_size_check( point );

    //xmin, ymin, xmax, ymax
    bbox[0] = std::min( point[0], bbox[0] );
    bbox[2] = std::max( point[0], bbox[2] );

    bbox[1] = std::min( point[1], bbox[1] );
    bbox[3] = std::max( point[1], bbox[3] );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::IntegerVector& point
  ) {
    bbox_size_check( point );

    Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( point );
    calculate_bbox( bbox, nv );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::IntegerMatrix& im
  ) {

    bbox_size_check( im );

    Rcpp::IntegerVector x = im( Rcpp::_, 0 );
    Rcpp::IntegerVector y = im( Rcpp::_, 1 );

    Rcpp::NumericVector nx = Rcpp::as< Rcpp::NumericVector >( x );
    Rcpp::NumericVector ny = Rcpp::as< Rcpp::NumericVector >( y );

    make_bbox( bbox, nx, ny );
  }

  template< int RTYPE >
  inline void calculate_bbox(
      Rcpp::Vector< RTYPE >& bbox,
      Rcpp::Matrix< RTYPE >& mat
  ) {
    bbox_size_check( mat );

    Rcpp::Vector< RTYPE > x = mat( Rcpp::_, 0 );
    Rcpp::Vector< RTYPE > y = mat( Rcpp::_, 1 );

    make_bbox( bbox, x, y );

  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::DataFrame& df
  ) {
    // assumes 'x' & 'y' column vectors
    bbox_size_check( df );

    Rcpp::NumericVector x = df[0];
    Rcpp::NumericVector y = df[1];

    make_bbox( bbox, x, y );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::DataFrame& df,
      Rcpp::IntegerVector& geometry_cols
  ) {

    geometries::utils::column_check( df, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::NumericVector x = df[ geometry_cols[0] ];
    Rcpp::NumericVector y = df[ geometry_cols[1] ];

    make_bbox( bbox, x, y );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::DataFrame& df,
      Rcpp::NumericVector& geometry_cols
  ) {
    geometries::utils::column_check( df, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::NumericVector x = df[ geometry_cols[0] ];
    Rcpp::NumericVector y = df[ geometry_cols[1] ];

    make_bbox( bbox, x, y );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::DataFrame& df,
      Rcpp::StringVector& geometry_cols
  ) {
    geometries::utils::column_check( df, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::String x_col = geometry_cols[0];
    Rcpp::String y_col = geometry_cols[1];

    Rcpp::NumericVector x = df[ x_col ];
    Rcpp::NumericVector y = df[ y_col ];

    make_bbox( bbox, x, y );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::IntegerMatrix& im,
      Rcpp::StringVector& geometry_cols
  ) {
    geometries::utils::column_check( im, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( im );
    calculate_bbox( bbox, df, geometry_cols );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::IntegerVector& iv,
      Rcpp::IntegerVector& geometry_cols
  ) {
    geometries::utils::column_check( iv, geometry_cols );
    bbox_size_check( geometry_cols );

    int x = iv[ geometry_cols[0] ];
    int y = iv[ geometry_cols[1] ];

    double dx = static_cast< double >( x );
    double dy = static_cast< double >( y );

    calculate_bbox( bbox, dx, dy );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& nv,
      Rcpp::IntegerVector& geometry_cols
  ) {
    geometries::utils::column_check( nv, geometry_cols );
    bbox_size_check( geometry_cols );

    double x = nv[ geometry_cols[0] ];
    double y = nv[ geometry_cols[1] ];

    calculate_bbox( bbox, x, y );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::IntegerMatrix& im,
      Rcpp::IntegerVector& geometry_cols
  ) {
    geometries::utils::column_check( im, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::IntegerVector x = im( Rcpp::_, geometry_cols[0] );
    Rcpp::IntegerVector y = im( Rcpp::_, geometry_cols[1] );

    Rcpp::NumericVector nx = Rcpp::as< Rcpp::NumericVector >( x );
    Rcpp::NumericVector ny = Rcpp::as< Rcpp::NumericVector >( y );

    make_bbox( bbox, nx, ny );
  }


  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::NumericMatrix& nm,
      Rcpp::StringVector& geometry_cols
  ) {
    geometries::utils::column_check( nm, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( nm );
    calculate_bbox( bbox, df, geometry_cols );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      Rcpp::NumericMatrix& nm,
      Rcpp::IntegerVector& geometry_cols
  ) {

    geometries::utils::column_check( nm, geometry_cols );
    bbox_size_check( geometry_cols );

    Rcpp::NumericVector x = nm( Rcpp::_, geometry_cols[0] );
    Rcpp::NumericVector y = nm( Rcpp::_, geometry_cols[1] );

    Rcpp::NumericVector nx = Rcpp::as< Rcpp::NumericVector >( x );
    Rcpp::NumericVector ny = Rcpp::as< Rcpp::NumericVector >( y );

    make_bbox( bbox, nx, ny );
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      SEXP& x
  ) {
    // assumes 2-column?, and in correct order?
    switch( TYPEOF( x ) ) {
    case INTSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
        calculate_bbox( bbox, im );
      } else {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
        calculate_bbox( bbox, iv );
      }
    break;
    }
    case REALSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
        calculate_bbox( bbox, nm );
      } else {
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
        calculate_bbox( bbox, nv );
      }
    break;
    }
    case VECSXP: {
      if( Rf_inherits( x, "data.frame" ) ) {
        Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
        calculate_bbox( bbox, df );
      } else if ( Rf_isNewList( x ) ) {
        Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
        R_xlen_t i;
        for( i = 0; i < lst.size(); ++i ) {
          SEXP s = lst[ i ];
          calculate_bbox( bbox, s );
        }
      }// else default
    break;
    }
    default: {
      Rcpp::stop("geometries - can't calculate bounding box for this type");
    }
    }
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      SEXP& x,
      Rcpp::IntegerVector& geometry_cols
  ) {

    switch( TYPEOF( x ) ) {
    case INTSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
        calculate_bbox( bbox, im, geometry_cols );
        break;
      } else {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
        calculate_bbox( bbox, iv, geometry_cols );
        break;
      }
    }
    case REALSXP: {
      if( Rf_isMatrix( x ) ) {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
        calculate_bbox( bbox, nm, geometry_cols );
        break;
      } else {
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
        calculate_bbox( bbox, nv, geometry_cols );
        break;
    }
    }
    case VECSXP: {
      if( Rf_inherits( x, "data.frame") ) {
        Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
        calculate_bbox( bbox, df, geometry_cols );
        break;
      }  else if ( Rf_isNewList( x ) ) {
        Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
        R_xlen_t i;
        for( i = 0; i < lst.size(); ++i ) {
          SEXP s = lst[ i ];
          calculate_bbox( bbox, s, geometry_cols );
        }
      }// else default
    break;
    }
    default: {
      Rcpp::stop("geometries - can't calculate bounding box for this type");
    }
    }
  }

  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      SEXP& x,
      Rcpp::StringVector& geometry_cols
  ) {
    switch( TYPEOF( x ) ) {
      case INTSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( x );
          calculate_bbox( bbox, im, geometry_cols );
          break;
        } else {
          Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( x );
          calculate_bbox( bbox, iv );
          break;
        }
      }
      case REALSXP: {
        if( Rf_isMatrix( x ) ) {
          Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( x );
          calculate_bbox( bbox, nm, geometry_cols );
          break;
        } else {
          Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( x );
          calculate_bbox( bbox, nv );
          break;
        }
      }
      case VECSXP: {
        if( Rf_inherits( x, "data.frame") ) {
          Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( x );
          calculate_bbox( bbox, df, geometry_cols );
          break;
        } else if ( Rf_isNewList( x ) ) {
          Rcpp::List lst = Rcpp::as< Rcpp::List >( x );
          R_xlen_t i;
          for( i = 0; i < lst.size(); ++i ) {
            SEXP s = lst[ i ];
            calculate_bbox( bbox, s, geometry_cols );
          }
        }// else default
      break;
      }
      default: {
        Rcpp::stop("geometries - can't calculate bounding box for this type");
      }
    }
  }


  inline void calculate_bbox(
      Rcpp::NumericVector& bbox,
      SEXP& x,
      SEXP& geometry_cols
  ) {

    if( Rf_isNull( geometry_cols ) ) {
      calculate_bbox( bbox, x );
      return;
    }

    // assumes 2-column?, and in correct order?
    switch( TYPEOF( geometry_cols ) ) {
      case REALSXP: {}
      case INTSXP: {
        Rcpp::IntegerVector iv_geometry_cols = Rcpp::as< Rcpp::IntegerVector >( geometry_cols );
        calculate_bbox( bbox, x, iv_geometry_cols );
        break;
      }
      case STRSXP: {
        Rcpp::StringVector sv_geometry_cols = Rcpp::as< Rcpp::StringVector >( geometry_cols );
        calculate_bbox( bbox, x, sv_geometry_cols );
        break;
      }
      default: {
        Rcpp::stop("geometries - can't calculate bounding box for this type");
      }
    }
  }

  inline Rcpp::NumericVector calculate_bbox(
    SEXP& x,
    SEXP& geometry_cols
  ) {
    Rcpp::NumericVector bbox(4);
    bbox(0) = bbox(1) = bbox(2) = bbox(3) = NA_REAL;
    calculate_bbox( bbox, x, geometry_cols );
    return bbox;
  }



} // bbox
} // geometries

#endif
