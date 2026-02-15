#ifndef GEOJSONSF_GEOJSON_WRITERS_H
#define GEOJSONSF_GEOJSON_WRITERS_H

#include <Rcpp.h>

namespace geojsonsf {
namespace writers {

  template< typename Writer >
  inline void start_feature_collection( Writer& writer ) {
    writer.String("type");
    writer.String("FeatureCollection");
    writer.String("features");
  }

  template< typename Writer >
  inline void start_features( Writer& writer ) {
    writer.String("type");
    writer.String("Feature");
  }

  template< typename Writer >
  inline void start_properties( Writer& writer ) {
    writer.String("properties");
  }

  template< typename Writer >
  inline void write_coordinate_string( Writer& writer ) {
    writer.String("coordinates");
  }

  template< typename Writer >
  inline void begin_geojson_geometry( Writer& writer, std::string& geom_type ) {
    writer.StartObject();
    writer.String("type");
    if (geom_type == "POINT") {
      writer.String("Point");
      write_coordinate_string( writer );
    } else if (geom_type == "MULTIPOINT") {
      writer.String("MultiPoint");
      write_coordinate_string( writer );
      writer.StartArray();
    } else if (geom_type == "LINESTRING") {
      writer.String("LineString");
      write_coordinate_string( writer );
      writer.StartArray();
    } else if (geom_type == "MULTILINESTRING") {
      writer.String("MultiLineString");
      write_coordinate_string( writer );
      writer.StartArray();
      writer.StartArray();
    } else if (geom_type == "POLYGON") {
      writer.String("Polygon");
      write_coordinate_string( writer );
      writer.StartArray();
      writer.StartArray();
    } else if (geom_type == "MULTIPOLYGON") {
      writer.String("MultiPolygon");
      write_coordinate_string( writer );
      writer.StartArray();
      writer.StartArray();
      writer.StartArray();
    } else if (geom_type == "GEOMETRYCOLLECTION") {
      writer.String("GeometryCollection");
      writer.String("geometries");
      writer.StartArray();
    }
  }

  template< typename Writer >
  inline void end_geojson_geometry(Writer& writer, std::string& geom_type) {
    if (geom_type == "POINT") {
      writer.EndObject();
    } else if (geom_type == "MULTIPOINT") {
      writer.EndArray();
      writer.EndObject();
    } else if (geom_type == "LINESTRING") {
      writer.EndArray();
      writer.EndObject();
    } else if (geom_type == "MULTILINESTRING") {
      writer.EndArray();
      writer.EndArray();
      writer.EndObject();
    } else if (geom_type == "POLYGON") {
      writer.EndArray();
      writer.EndArray();
      writer.EndObject();
    } else if (geom_type == "MULTIPOLYGON") {
      writer.EndArray();
      writer.EndArray();
      writer.EndArray();
      writer.EndObject();
    } else if (geom_type == "GEOMETRYCOLLECTION") {
      writer.EndArray();
      writer.EndObject();
    }
  }

  template< typename Writer >
  inline void polygon_separator( Writer& writer, R_xlen_t i, R_xlen_t n ) {
    if (i < ( n - 1 ) ) {
      writer.EndArray();
      writer.EndArray();
      writer.StartArray();
      writer.StartArray();
    }
  }

  template< typename Writer >
  inline void line_separator( Writer& writer, R_xlen_t i, R_xlen_t n) {
    if ( i < ( n - 1 ) ) {
      writer.EndArray();
      writer.StartArray();
    }
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, Rcpp::IntegerVector& point, int digits ) {
  	R_xlen_t n = point.size();
  	R_xlen_t i;
    int value;
    writer.StartArray();
    for ( i = 0; i < n; ++i ) {
    	value = point[i];
    	if( R_IsNA( value ) ) {
    		writer.Null();
    	} else {
        writer.Int( point[i] );
    	}
    }
    writer.EndArray();
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, Rcpp::IntegerVector& point ) {
  	points_to_geojson( writer, point, -1 );
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, Rcpp::NumericVector& point, int digits ) {
  	R_xlen_t n = point.size();
  	R_xlen_t i;
    double value;
    writer.StartArray();
    for ( i = 0; i < n; ++i ) {
      value = point[i];

    	if( R_IsNA( value ) ) {
    		writer.Null();
    	} else {

	    	if ( digits >= 0 ) {
	    		double e = std::pow( 10.0, digits );
	    		value = round( value * e ) / e;
	    	}
	    	writer.Double( value );
    	}
    }
    writer.EndArray();
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, Rcpp::NumericVector& point ) {
  	points_to_geojson( writer, point, -1 );
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, SEXP& point, int digits ) {
    switch( TYPEOF( point ) ) {
    case INTSXP: {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( point );
      points_to_geojson( writer, iv, digits );
      break;
    }
    case REALSXP: {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( point );
      points_to_geojson( writer, nv, digits );
    break;
    }
    }
  }

  template< typename Writer >
  inline void points_to_geojson( Writer& writer, SEXP& point ) {
  	points_to_geojson( writer, point, -1);
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, Rcpp::IntegerMatrix& line, int digits ) {
  	R_xlen_t i;
  	R_xlen_t nrow = line.nrow();
    for ( i = 0; i < nrow; ++i ) {
      Rcpp::IntegerVector this_row = line(i, Rcpp::_ );
      points_to_geojson( writer, this_row, digits );
    }
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, Rcpp::IntegerMatrix& line ) {
  	linestring_to_geojson( writer, line, -1);
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, Rcpp::NumericMatrix& line, int digits ) {
  	R_xlen_t i;
  	R_xlen_t nrow = line.nrow();
    for ( i = 0; i < nrow; ++i ) {
      Rcpp::NumericVector this_row = line(i, Rcpp::_ );
      points_to_geojson( writer, this_row, digits );
    }
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, Rcpp::NumericMatrix& line ) {
  	linestring_to_geojson( writer, line, -1 );
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, SEXP& line, int digits ) {
    switch( TYPEOF( line ) ) {
    case INTSXP: {
      Rcpp::IntegerMatrix iv = Rcpp::as< Rcpp::IntegerMatrix >( line );
      linestring_to_geojson( writer, iv, digits );
      break;
    }
    case REALSXP: {
      Rcpp::NumericMatrix nv = Rcpp::as< Rcpp::NumericMatrix >( line );
      linestring_to_geojson( writer, nv, digits );
      break;
    }
    }
  }

  template< typename Writer >
  inline void linestring_to_geojson( Writer& writer, SEXP& line ) {
  	linestring_to_geojson( writer, line, -1 );
  }

  template< typename Writer >
  inline void polygon_to_geojson( Writer& writer, Rcpp::List& sfg, int digits ) {
  	R_xlen_t i;
  	R_xlen_t n = sfg.size();
    for ( i = 0; i < n; ++i ) {
      Rcpp::NumericMatrix sfgi = sfg[i];
      linestring_to_geojson( writer, sfgi, digits );
      line_separator( writer, i, n );
    }
  }

  template< typename Writer >
  inline void polygon_to_geojson( Writer& writer, Rcpp::List& sfg ) {
  	polygon_to_geojson( writer, sfg, -1);
  }

  template< typename Writer >
  inline void multi_polygon_to_geojson( Writer& writer, Rcpp::List& sfg, int digits ) {
  	R_xlen_t i;
  	R_xlen_t n = sfg.size();
    for ( i = 0; i < n; ++i ) {
      Rcpp::List sfgi = sfg[i];
      polygon_to_geojson( writer, sfgi, digits );
      polygon_separator( writer, i, n );
    }
  }

  template< typename Writer >
  inline void multi_polygon_to_geojson( Writer& writer, Rcpp::List& sfg ) {
  	multi_polygon_to_geojson( writer, sfg, -1);
  }

} // namespace writers
} // namespace geojsonsf

#endif
