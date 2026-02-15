#ifndef JSONIFY_WRITERS_SCALARS_H
#define JSONIFY_WRITERS_SCALARS_H


namespace jsonify {
namespace writers {
namespace scalars {
  
  // ---------------------------------------------------------------------------
  // scalar values
  // ---------------------------------------------------------------------------
  template <typename Writer>
  inline void write_value( Writer& writer, const char* value ) {
    writer.String( value );
  }
  
  template <typename Writer>
  inline void write_value( Writer& writer, int& value ) {
    if( std::isnan( value ) ) {
      writer.Null();
    } else {
      writer.Int( value );
    }
  }
  
  template <typename Writer>
  inline void write_value( Writer& writer, double& value, int& digits ) {
    
    if(std::isnan( value ) ) {
      writer.Null();
    } else if ( std::isinf( value ) ) {
      
      std::string str = std::to_string( value );
      // https://stackoverflow.com/a/14744016/5977215
      if ( str[0] == '-') { 
        str[1] = toupper( str[1] );
      } else {
        str[0] = toupper(str[0]);
      }
      writer.String( str.c_str() );
    } else {
      
      if ( digits >= 0 ) {
        double e = std::pow( 10.0, digits );
        value = round( value * e ) / e;
      }
      writer.Double( value );
    }
  }
  
  template< typename Writer> 
  inline void write_value( Writer& writer, bool& value ) {
    writer.Bool( value );
  }
  
} // namespace scalars
} // naespace writers
} // namespace jsonify

#endif
