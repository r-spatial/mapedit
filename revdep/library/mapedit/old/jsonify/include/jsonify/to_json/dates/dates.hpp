#ifndef JSONIFY_DATES_H
#define JSONIFY_DATES_H

#include <Rcpp.h>

namespace jsonify {
namespace dates {

  inline bool is_in( const char* x, Rcpp::CharacterVector v ) {
    R_xlen_t n = v.size();
    R_xlen_t i;
    for( i = 0; i < n; ++i ) {
      if( v[i] == x ) {
        return true;
      }
    }
    return false;
  }

  inline std::string format_date( Rcpp::Date& d ) {
    int yyyy = d.getYear();
    int mm = d.getMonth();
    int dd = d.getDay();
    std::ostringstream os;
    os << std::setfill('0') << std::setw(4) << yyyy << "-";
    os << std::setfill('0') << std::setw(2) << mm << "-";
    os << std::setfill('0') << std::setw(2) << dd;
    return os.str();
  }

  inline std::string format_datetime( Rcpp::Datetime& d ) {
    int yyyy = d.getYear();
    int mm = d.getMonth();
    int dd = d.getDay();
    int h = d.getHours();
    int m = d.getMinutes();
    int s = d.getSeconds();
    std::ostringstream os;
    //sprintf( res, "%04d-%02d-%02dT%02d:%02d:%02d", yyyy, mm, dd, h, m, s);
    os << std::setfill('0') << std::setw(4) << yyyy << "-";
    os << std::setfill('0') << std::setw(2) << mm << "-";
    os << std::setfill('0') << std::setw(2) << dd << "T";
    os << std::setfill('0') << std::setw(2) << h << ":";
    os << std::setfill('0') << std::setw(2) << m << ":";
    os << std::setfill('0') << std::setw(2) << s;
    return os.str();
  }

  inline Rcpp::StringVector date_to_string( Rcpp::IntegerVector& iv ) {
    
    R_xlen_t i;
    R_xlen_t n = iv.size();
    Rcpp::StringVector sv( n );
    
    for ( i = 0; i < n; ++i ) {
      Rcpp::Date d = iv[i];
      sv[i] = format_date( d );
    }
    return sv;
  }

  inline Rcpp::StringVector date_to_string( Rcpp::NumericVector& nv ) {
    
    R_xlen_t i;
    R_xlen_t n = nv.size();
    Rcpp::StringVector sv( n );
    
    for ( i = 0; i < n; ++i ) {
      Rcpp::Date d = nv[i];
      sv[i] = format_date( d );
    }
    return sv;
  }

  inline Rcpp::StringVector posixct_to_string( Rcpp::IntegerVector& iv ) {
    
    R_xlen_t i;
    R_xlen_t n = iv.size();

    Rcpp::StringVector sv( n );
    
    for( i = 0; i < n; ++i ) {
      Rcpp::Datetime d = iv[ i ];
      sv[i] = format_datetime( d );
    }
    return sv;
  }
  
  inline Rcpp::StringVector posixct_to_string( Rcpp::NumericVector nv ) {
    R_xlen_t i;
    R_xlen_t n = nv.size();
    
    Rcpp::StringVector sv( n );
    
    for( i = 0; i < n; ++i ) {
      Rcpp::Datetime d = nv[ i ];
      sv[i] = format_datetime( d );
    }
    return sv;
  }


} // namespace format
} // namespace jsonify


#endif 
