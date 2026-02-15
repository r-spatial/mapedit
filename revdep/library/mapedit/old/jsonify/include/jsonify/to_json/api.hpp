#ifndef JSONIFY_TO_JSON_API_H
#define JSONIFY_TO_JSON_API_H

#include <Rcpp.h>
#include "jsonify/to_json/utils.hpp"
#include "jsonify/to_json/writers/complex.hpp"

namespace jsonify {
namespace api {

  inline Rcpp::StringVector to_json(
    SEXP lst, 
    bool unbox = false, 
    int digits = -1, 
    bool numeric_dates = true, 
    bool factors_as_string = true, 
    std::string by = "row"
  ) {
    rapidjson::StringBuffer sb;
    rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
    jsonify::writers::complex::write_value( writer, lst, unbox, digits, numeric_dates, factors_as_string, by );
    return jsonify::utils::finalise_json( sb );
  }

  // inline Rcpp::StringVector to_ndjson(
  //   Rcpp::DataFrame& df,
  //   bool unbox = false,
  //   int digits = -1,
  //   bool numeric_dates = true,
  //   bool factors_as_string = true,
  //   std::string by = "row"
  // ) {
  //   // loop over rows or columns, 
  // }
  
  inline void to_ndjson(
      Rcpp::DataFrame& df,
      std::ostringstream& os,
      bool unbox = false,
      int digits = -1,
      bool numeric_dates = true,
      bool factors_as_string = true,
      std::string by = "row"
  ) {
    R_xlen_t n_row = df.nrow();
    R_xlen_t n_cols = df.ncol();
    R_xlen_t df_col;
    R_xlen_t row;
    Rcpp::StringVector column_names = df.names();
    bool in_data_frame = true;
    
    //std::ostringstream os; // for storing the final string of ndjson
    
    if( by == "row" ) {
      
      for( row = 0; row < n_row; ++row ) {
        
        // create new stream each row
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        
        writer.StartObject();
        for( df_col = 0; df_col < n_cols; ++df_col ) {
          
          const char *h = column_names[ df_col ];
          writer.String( h );
          
          SEXP this_vec = df[ h ];
          
          switch( TYPEOF( this_vec ) ) {
          
          case VECSXP: {
            Rcpp::List lst = Rcpp::as< Rcpp::List >( this_vec );
            jsonify::writers::complex::write_value(
              writer, lst, unbox, digits, numeric_dates, factors_as_string, by, row, in_data_frame
              );
            break;
          }
          default: {
            jsonify::writers::complex::switch_vector(
              writer, this_vec, unbox, digits, numeric_dates, factors_as_string, row
            );
          }
          } // end switch
          
        } // end for
        writer.EndObject();
        
        os << sb.GetString();
        os << '\n';
      }  // end for (row)
      
    } else {
      // by == "column"
      for( df_col = 0; df_col < n_cols; ++df_col ) {
        // create new stream each row
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        
        writer.StartObject();
        
        const char *h = column_names[ df_col ];
        writer.String( h );
        SEXP this_vec = df[ h ];
        jsonify::writers::complex::write_value( writer, this_vec, unbox, digits, numeric_dates, factors_as_string, by, -1, in_data_frame );
        
        writer.EndObject();
        
        os << sb.GetString();
        os << '\n';
      }  // end for (column)
      
    }
    
    // Rcpp::StringVector sv = os.str();
    // return sv;
  }

  inline void to_ndjson(
      Rcpp::LogicalMatrix& mat,
      std::ostringstream& os,
      bool unbox = false,
      std::string by = "row"
  ) {
    
    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;
    
    //std::ostringstream os; // for storing the final string of ndjson
    
    if( by == "row" ) {
      
      for( i = 0; i < n_row; ++i ) {
        // create new stream each row
        Rcpp::LogicalVector v = mat( i, Rcpp::_ );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else if ( by == "column" ) {
      
      for( i = 0; i < n_col; ++i ) {
        
        Rcpp::LogicalVector v = mat( Rcpp::_, i );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else {
      Rcpp::stop("jsonify - expecting matrix operatinos by row or column");
    }
    // Rcpp::StringVector sv = os.str();
    // return sv;
  }

  inline void to_ndjson(
      Rcpp::IntegerMatrix& mat,
      std::ostringstream& os,
      bool unbox = false,
      std::string by = "row"
  ) {
    
    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;
    
    //std::ostringstream os; // for storing the final string of ndjson
    
    if( by == "row" ) {
      
      for( i = 0; i < n_row; ++i ) {
        // create new stream each row
        Rcpp::IntegerVector v = mat( i, Rcpp::_ );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else if ( by == "column" ) {
      
      for( i = 0; i < n_col; ++i ) {
        
        Rcpp::IntegerVector v = mat( Rcpp::_, i );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else {
      Rcpp::stop("jsonify - expecting matrix operatinos by row or column");
    }
    // Rcpp::StringVector sv = os.str();
    // return sv;
  }
  
  
  inline void to_ndjson(
    Rcpp::NumericMatrix& mat,
    std::ostringstream& os,
    bool unbox = false,
    int digits = -1,
    std::string by = "row"
  ) {
    
    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;
    
    //std::ostringstream os; // for storing the final string of ndjson
    
    if( by == "row" ) {
      
      for( i = 0; i < n_row; ++i ) {
        // create new stream each row
        Rcpp::NumericVector v = mat( i, Rcpp::_ );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox, digits );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else if ( by == "column" ) {
      
      for( i = 0; i < n_col; ++i ) {
        
        Rcpp::NumericVector v = mat( Rcpp::_, i );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox, digits );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else {
      Rcpp::stop("jsonify - expecting matrix operatinos by row or column");
    }
    // Rcpp::StringVector sv = os.str();
    // return sv;
  }

  inline void to_ndjson(
      Rcpp::StringMatrix& mat,
      std::ostringstream& os,
      bool unbox = false,
      std::string by = "row"
  ) {
    
    R_xlen_t n_row = mat.nrow();
    R_xlen_t n_col = mat.ncol();
    R_xlen_t i;
    
    //std::ostringstream os; // for storing the final string of ndjson
    
    if( by == "row" ) {
      
      for( i = 0; i < n_row; ++i ) {
        // create new stream each row
        Rcpp::StringVector v = mat( i, Rcpp::_ );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else if ( by == "column" ) {
      
      for( i = 0; i < n_col; ++i ) {
        
        Rcpp::StringVector v = mat( Rcpp::_, i );
        
        rapidjson::StringBuffer sb;
        rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
        jsonify::writers::simple::write_value( writer, v, unbox );
        
        os << sb.GetString();
        os << '\n';
      }
      
    } else {
      Rcpp::stop("jsonify - expecting matrix operations by row or column");
    }
    // Rcpp::StringVector sv = os.str();
    // return sv;
  }

  inline void to_ndjson(
      Rcpp::List& lst,
      std::ostringstream& os,
      bool unbox = false,
      int digits = -1,
      bool numeric_dates = true,
      bool factors_as_string = true,
      std::string by = "row"
  ) {
    R_xlen_t n = lst.size();
    R_xlen_t i;
    bool has_names = lst.hasAttribute("names");
    
    Rcpp::StringVector list_names;
    if( has_names ) {
      list_names = lst.names();
    }
    
    for( i = 0; i < n; ++i ) {
      rapidjson::StringBuffer sb;
      rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
      SEXP s = lst[ i ];
      
      if( has_names ) {
        writer.StartObject();
        const char *h = list_names[ i ];
        writer.String( h );
      }
      jsonify::writers::complex::write_value( writer, s, unbox, digits, numeric_dates, factors_as_string, by );
      if( has_names ) {
        writer.EndObject();
      }
      os << sb.GetString();
      os << '\n';
    }
    
  }

  template < int RTYPE >
  inline void to_ndjson(
      Rcpp::Vector< RTYPE > obj,
      std::ostringstream& os,
      bool unbox = false,
      int digits = -1,
      bool numeric_dates = true,
      bool factors_as_string = true
      ) {
    
    rapidjson::StringBuffer sb;
    rapidjson::Writer < rapidjson::StringBuffer > writer( sb );
    jsonify::writers::simple::write_value( writer, obj, unbox, digits, numeric_dates, factors_as_string );
    os << sb.GetString();
    os << '\n';
    
  }

  // lists are non-recursive; only the first element is ndjsonified...
  inline Rcpp::StringVector to_ndjson(
    SEXP& obj,
    bool unbox = false,
    int digits = -1,
    bool numeric_dates = true,
    bool factors_as_string = true,
    std::string by = "row"
  ) {

    std::ostringstream os; // for storing the final string of ndjson
    
    switch( TYPEOF( obj ) ) {
    case LGLSXP: {
      if( !Rf_isMatrix( obj ) ) {

        to_ndjson< LGLSXP >( obj, os, unbox, digits, numeric_dates, factors_as_string );
          
      } else {
        Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( obj );
        to_ndjson( lm, os, unbox, by );
      }
      break;
    }
    case INTSXP: {
      if( !Rf_isMatrix( obj ) ) {
      
        to_ndjson< INTSXP >( obj, os, unbox, digits, numeric_dates, factors_as_string );
        
      } else {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( obj );
        to_ndjson( im, os, unbox, by );

      }
      break;
    }
    case REALSXP: {
      if( !Rf_isMatrix( obj ) ) {
      
        to_ndjson< REALSXP >( obj, os, unbox, digits, numeric_dates, factors_as_string );
        
      } else {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( obj );
        to_ndjson( nm, os, unbox, digits, by );
        
      }
      break;
    }
    case STRSXP: {
      if( !Rf_isMatrix( obj ) ) {
        
        to_ndjson< STRSXP >( obj, os, unbox, digits, numeric_dates, factors_as_string );
        
      } else {
        Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( obj );
        to_ndjson( sm, os, unbox, by );
        
      }
      break;
    }
    case VECSXP: {
      if( Rf_inherits( obj, "data.frame") ) {
      
        Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( obj );
        to_ndjson( df, os, unbox, digits, numeric_dates, factors_as_string, by );
      
      } else {
        // list
        Rcpp::List lst = Rcpp::as< Rcpp::List >( obj );
        to_ndjson( lst, os, unbox, digits, numeric_dates, factors_as_string, by );

      }
      break;
    }
    default: {
      Rcpp::stop("jsonify - expecting a matrix, data.frame or list");
      }
    }
    
    // is this copy expensive?
    std::string res = os.str();
    res.pop_back(); // remove final \n
    Rcpp::StringVector js = res.c_str();
    js.attr("class") = "ndjson";
    return js;
  }


} // namespace api
} // namespace jsonify

#endif
