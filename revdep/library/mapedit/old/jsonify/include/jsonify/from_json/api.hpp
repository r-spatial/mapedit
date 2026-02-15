#ifndef JSONIFY_FROM_JSON_API_H
#define JSONIFY_FROM_JSON_API_H

#include <Rcpp.h>
#include "jsonify/from_json/from_json.hpp"

namespace jsonify {
namespace api {

  //' Parse JSON String
  //'
  //' Takes a JSON string as input, returns an R list of key-value pairs
  //'
  //' @param json const char, JSON string to be parsed. Coming from R, this
  //'  input should be a character vector of length 1.
  //' @export
  inline SEXP from_json( rapidjson::Value& doc, bool& simplify, bool& fill_na ) {

    // If the input is a scalar value of type int, double, string, or bool, 
    // return Rcpp vector with length 1.
    if( doc.IsInt() ) {
      return Rcpp::wrap( doc.GetInt() );
    }
    
    if( doc.IsDouble() ) {
      return Rcpp::wrap( doc.GetDouble() );
    }
    
    if( doc.IsString() ) {
      return Rcpp::wrap( Rcpp::String( doc.GetString() ) );
    }
    
    if( doc.IsBool() ) {
      return Rcpp::wrap( doc.GetBool() );
    }
    
    return jsonify::from_json::from_json( doc, simplify, fill_na );
  }

  inline SEXP from_json( const char* json, bool& simplify, bool& fill_na ) {
    rapidjson::Document doc;
    doc.Parse(json);

    // Make sure there were no parse errors
    if(doc.HasParseError()) {
      Rcpp::stop("json parse error");
    }
    
    return from_json( doc, simplify, fill_na );
  }

  inline SEXP from_ndjson( const char * ndjson, bool& simplify, bool& fill_na ) {
    
    // TODO:
    // - if ndjson is a single json object, no need to wrap in `[]` as this will nest it deeper
    rapidjson::Document doc;
    doc.Parse(ndjson);
    
    std::string json;
    
    if(doc.HasParseError()) {
      std::ostringstream os;
      os << '[';
      os << ndjson;
      os << ']';
      json = os.str();
      std::replace( json.begin(), json.end(), '\n', ',');
      return from_json( json.c_str(), simplify, fill_na );
    } 
    
    return from_json( doc, simplify, fill_na );
    
  }

}  // namespace api
}  // namespace jsonify

#endif
