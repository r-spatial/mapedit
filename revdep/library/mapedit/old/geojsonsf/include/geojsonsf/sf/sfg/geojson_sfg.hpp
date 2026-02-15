#ifndef GEOJSONSF_SFG_H
#define GEOJSONSF_SFG_H


#include <Rcpp.h>
#include "rapidjson/document.h"
#include "geojsonsf/sf/sfc/geojson_sfc.hpp"
#include "geojsonsf/geojson/geojson_validate.hpp"

#include "geometries/bbox/bbox.hpp"
#include "geometries/utils/attributes/attributes.hpp"
//#include "sfheaders/sfc/bbox.hpp"

#include "sfheaders/sfc/zm_range.hpp"
#include "sfheaders/sfg/sfg_dimension.hpp"
//#include "sfheaders/sfg/sfg_attributes.hpp"

using namespace rapidjson;

namespace geojsonsf {
namespace sfg {

	inline double get_lon(const Value& coord_array) {
		geojsonsf::validate::validate_point(coord_array[0]);
		return coord_array[0].GetDouble();
	}

	inline double get_lat(const Value& coord_array) {
		geojsonsf::validate::validate_point(coord_array[1]);
		return coord_array[1].GetDouble();
	}

	inline void get_integer_points(
			const Value& point_array,
			R_xlen_t& n,
			Rcpp::IntegerVector iv
      ) {
		R_xlen_t i;
		for ( i = 0; i < n; ++i ) {
			iv[i] = point_array[i].GetDouble();
		}
	}

	inline void get_numeric_points(
			const Value& point_array,
			R_xlen_t& n,
			Rcpp::NumericVector nv,
	    Rcpp::NumericVector& bbox,
	    Rcpp::NumericVector& z_range,
	    Rcpp::NumericVector& m_range
  ) {
		R_xlen_t i;
		for ( i = 0; i < n; ++i ) {
			geojsonsf::validate::validate_point(point_array[i]);
			nv[i] = point_array[i].GetDouble();
		}

		geometries::bbox::calculate_bbox( bbox, nv );
		std::string xyzm;
		sfheaders::zm::calculate_zm_ranges( z_range, m_range, nv, xyzm );
	}

	inline void get_points(
			const Value& point_array,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			Rcpp::List& sfc,
			R_xlen_t& i,
			bool requires_attribute,
			std::string attribute
  ) {
		R_xlen_t n = point_array.Size();
		geojsonsf::validate::validate_points(point_array);

		Rcpp::NumericVector nv( n );
		get_numeric_points( point_array, n, nv, bbox, z_range, m_range );

		if ( requires_attribute ) {
			std::string dim = sfheaders::sfg::sfg_dimension( n );

			Rcpp::StringVector class_attribute = { dim.c_str(), attribute.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( nv, atts );

		}
		sfc[i] = nv;

	}

	inline void get_line_string(
			const Value& line_array,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			Rcpp::List& sfc,
			R_xlen_t& i,
			bool requires_attribute,
			std::string attribute,
			R_xlen_t& max_cols
  ) {

		R_xlen_t n = line_array.Size();
		R_xlen_t row;

		Rcpp::NumericMatrix nm( n, 4 );

		for( row = 0; row < n; row++ ) {
			const Value& coord_array = line_array[ row ];
			geojsonsf::validate::validate_array( coord_array );

			R_xlen_t n_points = coord_array.Size();

			if( n_points <= 1 ) {
				Rcpp::stop("mis-specified geometry");
			}

			if( n_points > max_cols ) {
				max_cols = n_points;
			}

			Rcpp::NumericVector nv( 4, Rcpp::NumericVector::get_na() );  // initialise with ZM , we remove later
			get_numeric_points( coord_array, n_points, nv, bbox, z_range, m_range );
			nm( row, Rcpp::_ ) = nv;
		}

		nm = nm( Rcpp::_, Rcpp::Range(0, ( max_cols - 1 ) ) );

		if ( requires_attribute ) {
			std::string dim = sfheaders::sfg::sfg_dimension( max_cols );

			Rcpp::StringVector class_attribute = { dim.c_str(), attribute.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( nm, atts );

		}
		sfc[i] = nm;
	}

	inline void get_multi_line_string(
			const Value& multi_line_array,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			Rcpp::List& sfc,
			R_xlen_t& i,
			bool requires_attribute,
			std::string attribute
  ) {

		R_xlen_t n = multi_line_array.Size();
		Rcpp::List ml( n );
		R_xlen_t j;
		R_xlen_t max_dimension = 2;
		for ( j = 0; j < n; ++j ) {
			R_xlen_t max_cols = 2;
			geojsonsf::validate::validate_array( multi_line_array[j] );
			get_line_string( multi_line_array[j], bbox, z_range, m_range, ml, j, false, attribute, max_cols );
			if( max_cols > max_dimension ) {
				max_dimension = max_cols;
			}
		}
		if( requires_attribute ) {
			std::string dim = sfheaders::sfg::sfg_dimension( max_dimension );

			Rcpp::StringVector class_attribute = { dim.c_str(), attribute.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( ml, atts );

		}
		sfc[i] = ml;
	}

	inline void get_polygon(
			const Value& polygon_array,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			Rcpp::List& sfc,
			R_xlen_t& i,
			bool requires_attribute,
			std::string attribute
  ) {

		R_xlen_t n = polygon_array.Size();
		Rcpp::List pl( n );
		R_xlen_t j;
		R_xlen_t max_dimension = 2;
		for ( j = 0; j < n; ++j ) {
			R_xlen_t max_cols = 2;
			geojsonsf::validate::validate_array( polygon_array[j] );
			get_line_string( polygon_array[j], bbox, z_range, m_range, pl, j, false, "", max_cols );
			if ( max_cols > max_dimension ) {
				max_dimension = max_cols;
			}
		}


		if( requires_attribute ) {
			std::string dim = sfheaders::sfg::sfg_dimension( max_dimension );

			Rcpp::StringVector class_attribute = { dim.c_str(), attribute.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( pl, atts );

		}
		sfc[i] = pl;
	}

	inline void get_multi_polygon(
			const Value& multi_polygon_array,
			Rcpp::NumericVector& bbox,
			Rcpp::NumericVector& z_range,
			Rcpp::NumericVector& m_range,
			Rcpp::List& sfc,
			R_xlen_t& i,
			bool requires_attribute,
			std::string attribute
  ) {

		R_xlen_t n = multi_polygon_array.Size();
		Rcpp::List mp( n );
		R_xlen_t j, k;
		R_xlen_t max_dimension = 2;

		for ( j = 0; j < n; ++j ) {
			const Value& polygon_array = multi_polygon_array[j];
			geojsonsf::validate::validate_array( polygon_array );
			R_xlen_t np = polygon_array.Size();
			Rcpp::List p( np );

			for ( k = 0; k < np; k++ ) {
				R_xlen_t max_cols = 2;
				geojsonsf::validate::validate_array( polygon_array[k] );
				get_line_string( polygon_array[k], bbox, z_range, m_range, p, k, false, "", max_cols );
				if( max_cols > max_dimension ) {
					max_dimension = max_cols;
				}
			}
			mp[j] = p;
		}

		if( requires_attribute ) {
			std::string dim = sfheaders::sfg::sfg_dimension( max_dimension );

			Rcpp::StringVector class_attribute = { dim.c_str(), attribute.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( mp, atts );

		}
		sfc[i] = mp;
	}

	inline void create_null_sfg(
			Rcpp::List& sfc,
			std::unordered_set< std::string >& geometry_types,
			R_xlen_t& nempty
  ) {
		std::string geom_type;
		std::string dim = "XY";
		if (geometry_types.size() == 0) {
			geom_type = "Point";
		} else {
			geom_type = *geometry_types.begin();
			// the 'set' stores the geometries alphabetically
			// If there are more than one geometries, does it really matter which one is
			// selected?
			// if there is only one type, we want to use that one, so selecting the 'begin'
			// is as good a method as any
		}
		geometry_types.insert(geom_type);
		transform(geom_type.begin(), geom_type.end(), geom_type.begin(), toupper);

		if (geom_type == "POINT" ) {

			Rcpp::NumericVector nullObj(2, NA_REAL);

			Rcpp::StringVector class_attribute = { dim.c_str(), geom_type.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( nullObj, atts );

			sfc[0] = nullObj;

		} else if (geom_type == "MULTIPOINT" || geom_type == "LINESTRING") {

			Rcpp::NumericMatrix nullObj;
			Rcpp::StringVector class_attribute = { dim.c_str(), geom_type.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( nullObj, atts );

			sfc[0] = nullObj;

		} else {

			Rcpp::List nullObj;
			Rcpp::StringVector class_attribute = { dim.c_str(), geom_type.c_str(), "sfg" };
			Rcpp::List atts = Rcpp::List::create(
				Rcpp::_["class"] = class_attribute
			);
			geometries::utils::attach_attributes( nullObj, atts );

			sfc[0] = nullObj;
		}
		nempty++;
	}


} // namespace sfg
} // namespace geojsonsf

#endif
