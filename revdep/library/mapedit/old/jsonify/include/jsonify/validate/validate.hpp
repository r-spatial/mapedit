#ifndef R_JSONIFY_VALIDATE_H
#define R_JSONIFY_VALIDATE_H

#include <Rcpp.h>
#include "rapidjson/document.h"

namespace jsonify {
namespace validate {

  inline bool validate_json( rapidjson::Document& d, const char* json ) {
    d.Parse( json );
    bool res = true;
    if( d.Parse( json ).HasParseError() ) {
      res = false;
    }
    return res;
  }

  inline bool validate_json( const char* json ) {
    rapidjson::Document doc;
    return validate_json( doc, json );
  }

} // namespace validate
} // namespace jsonify

#endif
