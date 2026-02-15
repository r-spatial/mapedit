
#ifndef R_SFHEADERS_DF_UTILS_H
#define R_SFHEADERS_DF_UTILS_H

#include "geometries/utils/vectors/vectors.hpp"

namespace sfheaders {
namespace utils {

  inline Rcpp::String unique_name( Rcpp::String this_name, Rcpp::StringVector& existing_names ) {
    int is_in = geometries::utils::where_is( this_name, existing_names );

    if( is_in != -1 ) {
      // the name already exists, so we need to uniqueify it
      int counter = 1;
      std::string new_name;
      do {                       // #nocov
        std::ostringstream os;
        os << this_name.get_cstring() << ".." << counter;
        new_name = os.str();
        is_in = geometries::utils::where_is( new_name, existing_names );
        counter += 1;
      } while ( is_in != -1 );
      this_name = new_name;
    }

    return this_name;
  }

  template <int RTYPE>
  inline Rcpp::CharacterVector sfgClass( Rcpp::Vector<RTYPE> v ) {
    return v.attr("class");
  }

  inline Rcpp::CharacterVector getSfgClass( SEXP sfg ) {
    switch( TYPEOF( sfg ) ) {
    case REALSXP:
      return sfgClass<REALSXP>( sfg );
    case VECSXP:
      return sfgClass<VECSXP>( sfg );
    case INTSXP:
      return sfgClass<INTSXP>( sfg );
    default: Rcpp::stop("unknown sf type");   // #nocov
    }
    return Rcpp::CharacterVector();
  }

  inline Rcpp::List make_dataframe(
      Rcpp::List& res,
      R_xlen_t& total_rows,
      Rcpp::StringVector& column_names
    ) {

    res.attr("class") = Rcpp::CharacterVector("data.frame");

    if( total_rows > 0 ) {
      Rcpp::IntegerVector rownames = Rcpp::seq( 1, total_rows );
      res.attr("row.names") = rownames;
    } else {
      res.attr("row.names") = Rcpp::IntegerVector(0);  // #nocov
    }

    res.attr("names") = column_names;
    return res;
  }

  inline Rcpp::List make_dataframe(
    Rcpp::List& res,
    R_xlen_t& total_rows
  ) {
    Rcpp::StringVector res_names = res.names();
    return make_dataframe( res, total_rows, res_names );
  }

} // utils
} // sfheaders

#endif
