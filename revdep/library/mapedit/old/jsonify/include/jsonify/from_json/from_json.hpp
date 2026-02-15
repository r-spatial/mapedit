#ifndef R_JSONIFY_FROM_JSON_H
#define R_JSONIFY_FROM_JSON_H

#include <Rcpp.h>
#include "from_json_utils.hpp"
#include "simplify/simplify.hpp"

namespace jsonify {
namespace from_json {

  template< typename T > SEXP parse_json( const T& json, bool simplify, bool fill_na );
  template< typename T > SEXP parse_object( const T& json, bool simplify, bool fill_na );
  template< typename T > SEXP parse_array( const T& json, bool simplify, bool fill_na );
  
  template< typename T >
  inline SEXP parse_array(
      const T& json,
      bool simplify,
      bool fill_na
  ) {
    
    // IFF the array doesn't contain anything other than scalars,
    // we can go straight to a vector
    // rapidjson::Value::ConstArray  arr = json.GetArray();
    std::unordered_set< int > dtypes = get_dtypes( json );
    
    // for( auto t : dtypes ) {
    //   Rcpp::Rcout << t << std::endl;
    // }
    
    // Rcpp::Rcout << "parse_array " << std::endl;
    R_xlen_t json_length = json.Size();
    Rcpp::List out( json_length );
    
    R_xlen_t i = 0;
    // if( simplify && dtypes.size() == 1 && contains_array( dtypes ) ) {
    //   // Rcpp::Rcout << "returning early" << std::endl;
    //   // return array_to_vector( json.GetArray(), simplify );
    // 
    //   // Rcpp::Rcout << "json size: " << json.Size() << std::endl;
    //   for( const auto& child : json.GetArray() ) {
    //     out[ i++ ] = array_to_vector( child.GetArray(), simplify );
    //   }
    //   // iff it's just an array of arrays,
    //   // loop through and convert to matrix
    // 
    //   // for( const auto& child : json.GetArray() ) {
    //   //   //Rcpp::List av = array_to_vector( json.GetArray(), simplify );
    //   //
    //   // }
    //   return out;
    //   //return list_to_matrix( out );
    // }
    
    
    for ( const auto& child : json.GetArray() ) {
      out[ i++ ] =  parse_json( child, simplify, fill_na ); // iterating here again makes another list 
    }
    return out;
  }
  
  template< typename T >
  inline SEXP parse_object(
      const T& json,
      bool simplify,
      bool fill_na
  ) {
    
    R_xlen_t json_length = json.Size();
    
    if ( json_length == 0 ) {
      return R_NilValue;
    }
    
    Rcpp::List out( json_length );
    Rcpp::CharacterVector names( json_length );
    R_xlen_t i = 0;
    
    // removed in 1.2.3 
    // https://github.com/Tencent/rapidjson/issues/162#issuecomment-341824061
  // #if __cplusplus >= 201703L
  //   for ( const auto& [key, value] : json.GetObject() ) {
  //     out[ i ] = parse_json( value, simplify, fill_na );
  //     names[ i++ ] = std::string( key );
  //   }
  // #else
    for ( const auto& key_value : json.GetObject() ) {
      out[ i ] = parse_json( key_value.value, simplify, fill_na );
      names[ i++ ] = std::string( key_value.name.GetString() );
    }
  // #endif
    out.attr("names") = names;
    return out;
  }
  
  template< typename T >
  inline SEXP parse_json(
      const T& json,
      bool simplify,
      bool fill_na
  ) {
    
    // Rcpp::Rcout << "parse_json" << std::endl;
    std::unordered_set< int > dtypes;
    R_xlen_t json_length = json.Size();
    
    switch( json.GetType() ) {
    
      case rapidjson::kNullType: {
        return R_NA_VAL;
        break;
      }
      case rapidjson::kFalseType: {}
      case rapidjson::kTrueType: {
        return Rcpp::wrap< bool >( json.GetBool() );
      }
      case rapidjson::kStringType: {
        size_t sl = json.GetStringLength();
        std::string s = std::string( json.GetString(), sl);
        return Rcpp::wrap( s );
      }
        // numeric
      case rapidjson::kNumberType: {
        // if( json.IsDouble() || json.IsUint64() || json.IsInt64() ) {
        //   return Rcpp::wrap< double >( json.GetDouble() );
        // } else {
        //   return Rcpp::wrap< int >( json.GetInt() );
        // }
        if( json.IsInt() ) {
          return Rcpp::wrap< int >( json.GetInt() );
        } else {
          return Rcpp::wrap< double >( json.GetDouble() );
        }
      }
      case rapidjson::kObjectType: {
        return parse_object( json, simplify, fill_na );
      }
      case rapidjson::kArrayType: {
        
        dtypes.clear();
        dtypes = get_dtypes( json );
        
        // if the type is ONLY arrays
        // can it be parsed, then simplifed to matrices straight away?
        
        // for( auto t : dtypes ) {
        //   Rcpp::Rcout << t << std::endl;
        // }
        
        // if( simplify && dtypes.size() == 1 && contains_array( dtypes ) ) {
        //   
        //   return parse_array( json, simplify, fill_na );
        //   //return list_to_matrix( arr );
          
        if( simplify && !contains_object_or_array( dtypes ) ) {
          // Rcpp::Rcout << "here" << std::endl;
          return array_to_vector( json.GetArray(), simplify );
        } else {
          // return parse_array( json, simplify, fill_na );
          Rcpp::List arr = parse_array( json, simplify, fill_na );
          if( simplify ) {
            return jsonify::from_json::simplify( arr, dtypes, json_length, fill_na );
          } else {
            return arr;
          }
        }
      }
      default: {
        Rcpp::stop("jsonify - case not handled");
      }
    }
    
    return R_NilValue;  // #nocov never reaches
  }
  
  template< typename T >
  inline SEXP from_json(
      const T& json,
      bool simplify,
      bool fill_na
  ) {
    
    int json_type = json.GetType();
    R_xlen_t json_length = json.Size();
    
    if(json_length == 0) {
      if( json_type == 4 ) {
        // array goes to list
        return Rcpp::List::create();
      } else {
        return R_NilValue;
      }
    }
    
    return parse_json( json, simplify, fill_na );
  }
  
  // Test array types
  // If it's an object, it's 'simplify' value will be a data.frame
  // if it's an array, this test finds the types inside the array
  inline Rcpp::IntegerVector test_dtypes( const char * json ) {
    
    rapidjson::Document doc;
    doc.Parse(json);
    
    std::unordered_set< int > dtypes;
    
    if(!doc.IsArray()) {
      return Rcpp::IntegerVector(0); // if not an array it doesn't call get_dtypes
    }
    
    dtypes = get_dtypes<rapidjson::Document>(doc);
    Rcpp::IntegerVector iv(dtypes.begin(), dtypes.end());
    return iv;
  }
  
} // namespace from_json
} // namespace jsonify

#endif
