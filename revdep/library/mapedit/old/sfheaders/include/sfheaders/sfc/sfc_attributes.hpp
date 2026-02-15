#ifndef R_SFHEADERS_SFC_ATTRIBUTES_H
#define R_SFHEADERS_SFC_ATTRIBUTES_H

#include "sfheaders/sfc/z_range.hpp"
#include "sfheaders/sfc/m_range.hpp"

#include "sfheaders/sfc/bbox.hpp"
//#include "geometries/bbox/bbox.hpp"

#include <Rcpp.h>

namespace sfheaders {
namespace sfc {

  const int SFC_POINT           = 1;
  const int SFC_MULTIPOINT      = 2;
  const int SFC_LINESTRING      = 3;
  const int SFC_MULTILINESTRING = 4;
  const int SFC_POLYGON         = 5;
  const int SFC_MULTIPOLYGON    = 6;

  // #nocov start
  template <int RTYPE>
  inline Rcpp::CharacterVector rClass( Rcpp::Vector<RTYPE> v ) {
    return v.attr("class");
  }

  inline Rcpp::CharacterVector getRClass( SEXP sf ) {

    switch( TYPEOF(sf) ) {
    case REALSXP:
      return rClass<REALSXP>( sf );
    case VECSXP:
      return rClass<VECSXP>( sf );
    case INTSXP:
      return rClass<INTSXP>( sf );
    }
    return "";
  }

  template <int RTYPE>
  inline Rcpp::CharacterVector sfClass( Rcpp::Vector<RTYPE> v ) {
    return v.attr("class");
  }

  inline Rcpp::CharacterVector getSfClass( SEXP sf ) {

    switch( TYPEOF(sf) ) {
    case REALSXP:
      return sfClass<REALSXP>( sf );
    case VECSXP:
      return sfClass<VECSXP>( sf );
    case INTSXP:
      return sfClass<INTSXP>( sf );
    default: Rcpp::stop("unknown sf type");
    }
    return "";
  }

  inline Rcpp::StringVector start_sfc_classes( R_xlen_t collectionCount ) {
    Rcpp::StringVector sfc_classes( collectionCount );
    return sfc_classes;
  }

  /* sfc class
   *
   * Determins the class of the sfc object
   * attr(sfc, "class") : c("sfc_POLYGON", "sfc")
   *
   */
  inline std::string sfc_class(
      Rcpp::List& sfc,
      std::string geom_type,
      std::unordered_set< std::string >& geometry_types
  ) {

    std::string geometry_class;
    int i;

    // handle no features
    // '{"type":"FeatureCollection","features":[]}'
    if (geometry_types.size() == 0 ) {
      return "GEOMETRY";
    }

    if (geom_type == "GEOMETRYCOLLECTION") {
      geometry_class = "GEOMETRYCOLLECTION";
    } else {

      if (geometry_types.size() > 1) {
        geometry_class = "GEOMETRY";

        Rcpp::StringVector sfc_classes = start_sfc_classes( sfc.size() );
        for ( i = 0; i < sfc.size(); ++i ) {
          SEXP sfci = sfc[i];
          Rcpp::CharacterVector cls = getSfClass( sfci );
          sfc_classes[i] = cls[1];
        }

        // attribute::classes
        sfc.attr("classes") = sfc_classes;

      } else {
        std::string type = *geometry_types.begin();
        transform(type.begin(), type.end(), type.begin(), toupper);
        geometry_class = type;
      }
    }
    return geometry_class;
  }
  // #nocov end

  // issue 49
  // fix crs
  inline void update_crs( Rcpp::List& crs ) {
    if( !crs.containsElementNamed("input") ) {
      Rcpp::String input = NA_STRING;
      crs["input"] = input;
    }

    if( !crs.containsElementNamed("wkt") ) {
      Rcpp::String wkt = NA_STRING;
      crs["wkt"] = wkt;
    }
  }

  /*
   * get sfc attributes
   *
   * extracts the attributes from an sfc object
   */
  inline Rcpp::List get_sfc_attributes(
    Rcpp::List& sfc
  ) {
    Rcpp::List crs = sfc.attr("crs");
    update_crs( crs );

    int n_empty = sfc.attr("n_empty");
    Rcpp::CharacterVector sfc_class = sfc.attr("class");
    double precision = sfc.attr("precision");
    Rcpp::NumericVector bbox = sfc.attr("bbox");

    Rcpp::NumericVector z_range = sfheaders::zm::start_z_range();
    Rcpp::NumericVector m_range = sfheaders::zm::start_m_range();
    if( sfc.hasAttribute("z_range") ) {
      z_range = sfc.attr("z_range");
    }
    if( sfc.hasAttribute("m_range") ) {
      m_range = sfc.attr("m_range");
    }

    return Rcpp::List::create(
      Rcpp::_["n_empty"] = n_empty,
      Rcpp::_["crs"] = crs,
      Rcpp::_["class"] = sfc_class,
      Rcpp::_["precision"] = precision,
      Rcpp::_["bbox"] = bbox,
      Rcpp::_["z_range"] = z_range,
      Rcpp::_["m_range"] = m_range
    );
  }

  /* Attach SFC attributes
   *
   * attaches the attributes required to make an sfc object
   *
   */
  inline void attach_sfc_attributes(
      Rcpp::List& sfc,
      Rcpp::CharacterVector& sfc_class,
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& z_range,
      Rcpp::NumericVector& m_range,
      Rcpp::List& crs,
      int n_empty = 0,
      double precision = 0.0
  ) {
    // attribute::n_empty
    sfc.attr("n_empty") = n_empty;

    // attribute::crs
    crs.attr("class") = Rcpp::CharacterVector::create("crs");
    sfc.attr("crs") = crs;

    sfc.attr("class") = sfc_class;

    // attribute::precision
    sfc.attr("precision") = precision;

    // attribute::bbox
    sfheaders::bbox::attach_bbox_attributes( bbox );
    sfc.attr("bbox") = bbox;

    sfheaders::zm::attach_z_range_attributes( z_range );

    if( !Rcpp::NumericVector::is_na( z_range[0] ) && !Rcpp::NumericVector::is_na( z_range[1] ) ) {
      sfc.attr("z_range") = z_range;
    }

    sfheaders::zm::attach_m_range_attributes( m_range );
    if( !Rcpp::NumericVector::is_na( m_range[0] ) && !Rcpp::NumericVector::is_na( m_range[1] ) ) {
      sfc.attr("m_range") = m_range;
    }
  }

  inline void attach_sfc_attributes(
      Rcpp::List& sfc,
      std::string& geom_type,
      std::unordered_set< std::string >& geometry_types,
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& z_range,
      Rcpp::NumericVector& m_range,
      Rcpp::List& crs,
      int n_empty = 0,
      double precision = 0.0
  ) {

    std::string geometry_class = sfc_class( sfc, geom_type, geometry_types );
    Rcpp::CharacterVector sfc_class = Rcpp::CharacterVector::create("sfc_" + geometry_class, "sfc");

    attach_sfc_attributes(
      sfc, sfc_class, bbox, z_range, m_range, crs, n_empty, precision
    );
  }

  // backwards compatibility (e.g. geojsonsf 1.3.3.)
  inline void attach_sfc_attributes(
      Rcpp::List& sfc,
      std::string& geom_type,
      std::unordered_set< std::string >& geometry_types,
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& z_range,
      Rcpp::NumericVector& m_range,
      int& epsg,
      Rcpp::String& proj4string,
      int n_empty = 0,
      double precision = 0.0
  ) {

    std::string geometry_class = sfc_class( sfc, geom_type, geometry_types );
    Rcpp::CharacterVector sfc_class = Rcpp::CharacterVector::create("sfc_" + geometry_class, "sfc");

    Rcpp::String input = NA_STRING;
    Rcpp::String wkt = NA_STRING;

    Rcpp::List crs = Rcpp::List::create(
      Rcpp::_["input"] = input,
      Rcpp::_["wkt"] = wkt,
      Rcpp::_("epsg") = epsg,
      Rcpp::_("proj4string") = proj4string
    );

    attach_sfc_attributes(
      sfc, sfc_class, bbox, z_range, m_range, crs, n_empty, precision
    );
  }

  inline void attach_sfc_attributes(
    Rcpp::List& sfc,
    Rcpp::List& attributes
  ) {

    // TODO
    // - check the attributes object has the correct names before selecting them
    int n_empty = attributes["n_empty"];
    Rcpp::List crs = attributes["crs"];
    Rcpp::CharacterVector sfc_class = attributes["class"];
    double precision = attributes["precision"];
    Rcpp::NumericVector bbox = attributes["bbox"];
    Rcpp::NumericVector z_range = attributes["z_range"];
    Rcpp::NumericVector m_range = attributes["m_range"];



    attach_sfc_attributes(
      sfc, sfc_class, bbox, z_range, m_range, crs, n_empty, precision
    );

  }

  inline SEXP create_sfc(
      Rcpp::List& sfc,
      std::string& geom_type,
      std::unordered_set< std::string >& geometry_types,
      Rcpp::NumericVector& bbox,
      Rcpp::NumericVector& z_range,
      Rcpp::NumericVector& m_range,
      Rcpp::List& crs,
      int n_empty = 0,
      double precision = 0.0
  ) {
    sfheaders::sfc::attach_sfc_attributes(
      sfc, geom_type, geometry_types, bbox, z_range, m_range, crs, n_empty, precision
    );
    return sfc;
  }

} // sfc
} // sfheaders


#endif
