#ifndef R_SFHEADERS_SFG_TYPES_H
#define R_SFHEADERS_SFG_TYPES_H

#include <Rcpp.h>

#include "sfheaders/sfg/sfg_attributes.hpp"
#include "sfheaders/sfg/sfg_dimension.hpp"

#include "geometries/utils/sexp/sexp.hpp"
#include "geometries/utils/attributes/attributes.hpp"

namespace sfheaders {
namespace sfg {

  const int SFG_POINT           = 1;
  const int SFG_MULTIPOINT      = 2;
  const int SFG_LINESTRING      = 3;
  const int SFG_MULTILINESTRING = 4;
  const int SFG_POLYGON         = 5;
  const int SFG_MULTIPOLYGON    = 6;

  const int VECTOR              = 1;
  const int MATRIX              = 2;
  const int LIST_MATRIX         = 3;
  const int LIST_LIST_MATRIX    = 4;

  inline std::string get_sfg_type( int sfg_type ) {
    switch( sfg_type ) {
    case SFG_POINT: { return "POINT"; }
    case SFG_MULTIPOINT: { return "MULTIPOINT"; }
    case SFG_LINESTRING: { return "LINESTRING"; }
    case SFG_MULTILINESTRING: { return "MULTILINESTRING"; }
    case SFG_POLYGON: { return "POLYGON"; }
    case SFG_MULTIPOLYGON: { return "MULTIPOLYGON"; }
    default: {
      Rcpp::stop("sfheaders - unknown sfg type");
    }
    }
    return ""; // never reaches
  }


  template < int RTYPE >
  inline void make_sfg(
      Rcpp::Vector< RTYPE >& vec,
      int sfg_type,
      std::string& xyzm
  ) {
    R_xlen_t n_col = vec.length();
    std::string dim = sfheaders::sfg::sfg_dimension( n_col, xyzm );

    std::string geom_type = get_sfg_type( sfg_type );

    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = Rcpp::CharacterVector::create( dim, geom_type, "sfg" )
    );
    geometries::utils::attach_attributes( vec, attributes );
  }


  template < int RTYPE >
  inline void make_sfg(
      Rcpp::Matrix< RTYPE >& mat,
      int sfg_type,
      std::string& xyzm
  ) {

    R_xlen_t n_col = mat.ncol();
    std::string dim = sfheaders::sfg::sfg_dimension( n_col, xyzm );

    std::string geom_type = get_sfg_type( sfg_type );

    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = Rcpp::CharacterVector::create( dim, geom_type, "sfg" )
    );
    geometries::utils::attach_attributes( mat, attributes );
  }

  inline void make_sfg(
      Rcpp::List& lst,
      int sfg_type,
      std::string& xyzm
  ) {
    std::string dim = sfheaders::sfg::sfg_dimension( lst, xyzm );

    std::string geom_type = get_sfg_type( sfg_type );

    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = Rcpp::CharacterVector::create( dim, geom_type, "sfg" )
    );
    geometries::utils::attach_attributes( lst, attributes );
  }

  inline void make_sfg(
    Rcpp::List& lst,
    R_xlen_t n_col,
    int sfg_type,
    std::string& xyzm
  ) {
    std::string dim = sfheaders::sfg::sfg_dimension( n_col, xyzm );

    std::string geom_type = get_sfg_type( sfg_type );

    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = Rcpp::CharacterVector::create( dim, geom_type, "sfg" )
    );
    geometries::utils::attach_attributes( lst, attributes );
  }

  inline void make_sfg(
    SEXP& x,
    R_xlen_t n_col,
    int sfg_type,
    std::string& xyzm
  ) {
    std::string dim = sfheaders::sfg::sfg_dimension( n_col, xyzm );
    std::string geom_type = get_sfg_type( sfg_type );
    Rcpp::List attributes = Rcpp::List::create(
      Rcpp::_["class"] = Rcpp::CharacterVector::create( dim, geom_type, "sfg" )
    );
    geometries::utils::attach_attributes( x, attributes );
  }

  inline void make_sfg(
    SEXP& x,
    int sfg_type,
    std::string& xyzm
  ) {
    R_xlen_t n_col = geometries::utils::sexp_n_col( x );
    make_sfg( x, n_col, sfg_type, xyzm );
  }


} // sfg
} // sfheaders


#endif
