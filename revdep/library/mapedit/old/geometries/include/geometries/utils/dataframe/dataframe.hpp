#ifndef R_GEOMETRIES_UTILS_DATAFRAME_H
#define R_GEOMETRIES_UTILS_DATAFRAME_H

namespace geometries {
namespace utils {

  inline Rcpp::DataFrame make_dataframe(
      Rcpp::List& res,
      R_xlen_t& total_rows,
      Rcpp::CharacterVector& column_names
  ) {

    res.attr("class") = Rcpp::CharacterVector("data.frame");

    if( total_rows > 0 ) {
      Rcpp::IntegerVector rownames = Rcpp::seq( 1, total_rows );
      res.attr("row.names") = rownames;
    } else {
      res.attr("row.names") = Rcpp::IntegerVector(0);
    }

    res.attr("names") = column_names;
    return res;

  }

} // utils
} // geometries

#endif
