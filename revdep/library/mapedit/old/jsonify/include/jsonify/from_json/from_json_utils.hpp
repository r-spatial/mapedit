#ifndef R_JSONIFY_FROM_JSON_UTILS_H
#define R_JSONIFY_FROM_JSON_UTILS_H

#include <Rcpp.h>

#include "rapidjson/document.h"

#define R_NA_VAL Rcpp::LogicalVector::create(NA_LOGICAL);

namespace jsonify {
namespace from_json {

  inline R_xlen_t where_is(
    Rcpp::String to_find,
    Rcpp::StringVector& sv
  ) {
    R_xlen_t n = sv.size();
    R_xlen_t i;

    for( i = 0; i < n; ++i ) {

      if ( to_find == sv[i] ) {
        return i;
      }
    }
    return -1;
  }

  // Iterate over a rapidjson object and get the unique data types of each value.
  // Save unique data types as ints to unordered_set dtypes.
  // Compatible with rapidjson::Array and rapidjson::Value.
  template< typename T >
  inline std::unordered_set< int > get_dtypes( T& doc ) {
    
    std::unordered_set< int > dtypes;
    
    R_xlen_t doc_len = doc.Size();

    int curr_dtype;
    R_xlen_t i;
    for(i = 0; i < doc_len; ++i) {
       curr_dtype = doc[i].GetType();
      // rapidjson uses separate ints for types true (2) and false (1)...combine
      // them into one value such that bool is 1.
      if(curr_dtype == 2) {
        curr_dtype = 1;
      }

      // rapidjson uses the same int for types double and int...split them up,
      // such that double is 9 and int is 8.
      if(curr_dtype == 6) {
        if(doc[i].IsDouble()) {
          curr_dtype = 9;
        } else {
          curr_dtype = 8;
        }
      }

      dtypes.insert(curr_dtype);
    }
    
    return dtypes;
  }

  inline bool contains_array( std::unordered_set< int >& dtypes ) {
    return dtypes.find(4) != dtypes.end();
  }
  
  inline bool contains_object( std::unordered_set< int >& dtypes ) {
    return dtypes.find(3) != dtypes.end();
  }
  
  inline bool contains_object_or_array( std::unordered_set< int >& dtypes ) {
    return contains_array( dtypes ) || contains_object( dtypes );
  }

  template <int RTYPE>
  inline R_xlen_t sexp_length(Rcpp::Vector<RTYPE> v) {
    return v.length();
  }
  
  inline R_xlen_t get_sexp_length( SEXP s ) {

    switch( TYPEOF(s) ) {
    case NILSXP: 
      return 0;
    case LGLSXP:
      return sexp_length< LGLSXP >( s );
    case REALSXP:
      return sexp_length< REALSXP >( s );
    case VECSXP:
      //return 2; // number bigger than 1 (vector)
      return sexp_length< VECSXP >( s );
    case INTSXP:
      return sexp_length< INTSXP >( s );
    case STRSXP:
      return sexp_length< STRSXP >( s );
    default: Rcpp::stop("jsonify - unknown vector type");
    }
    return 0;
  }
  
  // // Convert all NULL elements in a list to NA.
  // inline void null_to_na(Rcpp::List& x) {
  //   for(unsigned int i = 0; i < x.size(); ++i) {
  //     if(Rf_isNull(x[i])) {
  //       x[i] = R_NA_VAL;
  //     }
  //   }
  // }
  
  // returns -1 if doens't exist
  // else the stored r_type
  // inline R_xlen_t column_value(
  //     std::unordered_map< std::string, int >& column_map,
  //     const char* to_find
  //   ) {
  //   std::string str( to_find );
  //   std::unordered_map< std::string, int >::iterator it;
  //   it = column_map.find( str );
  //   
  //   if( it != column_map.end() ) {
  //     R_xlen_t res = it->second;
  //     return res;
  //   }
  //   return -1;
  // }
  
  inline R_xlen_t column_value(
      std::unordered_map< std::string, R_xlen_t >& column_map,
      const char* to_find
  ) {
    std::string str( to_find );
    std::unordered_map< std::string, R_xlen_t >::iterator it;
    it = column_map.find( str );
    
    if( it != column_map.end() ) {
      R_xlen_t res = it->second;
      return res;
    }
    return -1;
  }

  inline void insert_column_value(
    Rcpp::List& columns,
    const char* this_column,
    SEXP& val,
    R_xlen_t& row_index
  ) {
    Rcpp::List lst = columns[ this_column ];
    lst[ row_index ] = val;
    Rcpp::StringVector n = columns.names();
    columns[ this_column ] = lst;
  }
  
  
  // here we don't actually care what the type is yet
  // as for now a 'column' is just a list
  inline void append_new_column(
      Rcpp::List& columns,
      const char* this_column,
      R_xlen_t& n_rows
  ) {
    Rcpp::List new_column( n_rows );
    columns[ this_column ] = new_column;
  }
  
  inline void append_new_column_fill_na(
    Rcpp::List& columns,
    const char* this_column,
    R_xlen_t& n_rows
  ) {
    Rcpp::List new_column( n_rows );
    // need NAs when fill_na = true;
    R_xlen_t i;
    for( i = 0; i < n_rows; ++i ) {
      new_column[i] = NA_LOGICAL;
    }
    columns[ this_column ] = new_column;
  }
  
} // namespace from_json
} // namespace jsonify

#endif
