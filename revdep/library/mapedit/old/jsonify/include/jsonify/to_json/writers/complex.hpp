#ifndef R_JSONIFY_WRITERS_COMPLEX_H
#define R_JSONIFY_WRITERS_COMPLEX_H

#include <Rcpp.h>
#include "jsonify/to_json/utils.hpp"
#include "jsonify/to_json/dates/dates.hpp"
#include "jsonify/to_json/writers/simple.hpp"
#include <math.h>

using namespace rapidjson;

namespace jsonify {
namespace writers {
namespace complex {

  template < typename Writer >
  inline void switch_vector(
      Writer& writer, 
      SEXP& this_vec, 
      bool& unbox, 
      int& digits, 
      bool& numeric_dates, 
      bool& factors_as_string
    ) {
    
    switch( TYPEOF( this_vec ) ) {
    case REALSXP: {
    if( Rf_isMatrix( this_vec ) ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( this_vec );
      jsonify::writers::simple::write_value( writer, nm, unbox );
    } else {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( this_vec );
      jsonify::writers::simple::write_value( writer, nv, unbox, digits, numeric_dates );
    }
    break;
    }
    case INTSXP: {
    if( Rf_isMatrix( this_vec ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( this_vec );
      jsonify::writers::simple::write_value( writer, im, unbox );
    } else {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( this_vec );
      jsonify::writers::simple::write_value( writer, iv, unbox, numeric_dates, factors_as_string );
    }
      break;
    }
    case LGLSXP: {
    if( Rf_isMatrix( this_vec ) ) {
      Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( this_vec );
      jsonify::writers::simple::write_value( writer, lm, unbox );
    } else {
        Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( this_vec );
        jsonify::writers::simple::write_value( writer, lv, unbox );
    }
    break;
    }
    default: {
    if( Rf_isMatrix( this_vec ) ) {
      Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( this_vec );
      jsonify::writers::simple::write_value( writer, sm, unbox );
    } else {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( this_vec );
      jsonify::writers::simple::write_value( writer, sv, unbox );
    }
    break;
    }
    }
  }
  
  // working by-row, so we only use a single element of each vector
  template < typename Writer >
  inline void switch_vector(
      Writer& writer, 
      SEXP& this_vec, 
      bool& unbox, 
      int& digits, 
      bool& numeric_dates, 
      bool& factors_as_string, 
      R_xlen_t& row
    ) {
    
    switch( TYPEOF( this_vec ) ) {
    case REALSXP: {
      
      if( Rf_isMatrix( this_vec ) ) {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( this_vec );
        jsonify::writers::simple::write_value( writer, nm, row, unbox );
      } else {
        
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( this_vec );
        jsonify::writers::simple::write_value( writer, nv, row, digits, numeric_dates );
      }
    break;
    }
    case INTSXP: {
      if( Rf_isMatrix( this_vec ) ) {
        
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( this_vec );
        jsonify::writers::simple::write_value( writer, im, row, unbox );
      } else {
        
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( this_vec );
        jsonify::writers::simple::write_value( writer, iv, row, numeric_dates, factors_as_string );
      }
      break;
    }
    case LGLSXP: {
      if( Rf_isMatrix( this_vec ) ) {
        Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( this_vec );
        jsonify::writers::simple::write_value( writer, lm, row, unbox );
     } else {
        Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( this_vec );
        jsonify::writers::simple::write_value( writer, lv, row );
      }
      break;
    }
    default: {
      if( Rf_isMatrix( this_vec ) ) {
        Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( this_vec );
        jsonify::writers::simple::write_value( writer, sm, row, unbox );
      } else {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( this_vec );
        jsonify::writers::simple::write_value( writer, sv, row );
      }
      break;
    }
    }
  }

  template< typename Writer >
  inline void write_value(
      Writer& writer, 
      SEXP list_element, 
      bool unbox = false, 
      int digits = -1, 
      bool numeric_dates = true,
      bool factors_as_string = true, 
      std::string by = "row", 
      R_xlen_t row = -1,   // for when we are recursing into a row of a data.frame
      bool in_data_frame = false  // for keeping track of when we're in a column of a data.frame
      ) {
    
    R_xlen_t i, df_col, df_row;
    
    if( Rf_isNull( list_element ) ) {
      writer.StartObject();
      writer.EndObject();
      return;
    } 
    
    if( Rf_isMatrix( list_element ) ) {
      
      switch( TYPEOF( list_element ) ) {
      case REALSXP: {
        Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( list_element );
        return jsonify::writers::simple::write_value( writer, nm, unbox, digits, by );
        break;
      }
      case INTSXP: {
        Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( list_element );
        return jsonify::writers::simple::write_value( writer, im, unbox, by );
        break;
      }
      case LGLSXP: {
        Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( list_element );
        return jsonify::writers::simple::write_value( writer, lm, unbox, by );
        break;
      }
      default :{
        Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( list_element );
        return jsonify::writers::simple::write_value( writer, sm, unbox, by );
        break;
      }
      }
    } else if ( Rf_inherits( list_element, "data.frame" ) ) {
      
      in_data_frame = true;
      Rcpp::DataFrame df = Rcpp::as< Rcpp::DataFrame >( list_element );
      R_xlen_t n_cols = df.ncol();
      R_xlen_t n_rows = df.nrows();
      Rcpp::StringVector column_names = df.names();
      
      // issue 59
      // moving the factor_as_string conersion as high up as possible, 
      // so it works on the whole vector. 
      if( factors_as_string ) {
        jsonify::utils::factors_to_string( df );
      }
      
      // issue 60
      if( !numeric_dates ) {
        int tp;

        for( df_col = 0; df_col < n_cols; ++df_col ) {
          const char * col_name = column_names[ df_col ];
          tp = TYPEOF( df[ col_name ] );

          if( tp == REALSXP ) {
            Rcpp::NumericVector nv_dte = Rcpp::as< Rcpp::NumericVector >( df[ col_name ] );
            Rcpp::CharacterVector cls = jsonify::utils::getRClass( nv_dte );

            if( jsonify::dates::is_in( "Date", cls ) ) {

              Rcpp::StringVector sv_dte = jsonify::dates::date_to_string( nv_dte );
              df[ col_name ] = sv_dte;

            } else if (jsonify::dates::is_in( "POSIXt", cls ) ) {

              Rcpp::StringVector sv_psx = jsonify::dates::posixct_to_string( nv_dte );
              df[ col_name ] = sv_psx;
            }
          }
        }
      }
      
      
      if ( by == "column") {
        writer.StartObject();
        
        for( df_col = 0; df_col < n_cols; ++df_col ) {

          const char *h = column_names[ df_col ];
          writer.String( h );
          SEXP this_vec = df[ h ];
          write_value( writer, this_vec, unbox, digits, numeric_dates, factors_as_string, by, -1, in_data_frame );
          
        }
        writer.EndObject();
        
      } else { // by == "row"
        
        if ( row >= 0 ) {
          
          writer.StartObject();
          for( df_col = 0; df_col < n_cols; df_col++ ) {
            
            const char *h = column_names[ df_col ];
            writer.String( h );
            
            SEXP this_vec = df[ h ];
            
            switch( TYPEOF( this_vec ) ) {
            
            case VECSXP: {
              Rcpp::List lst = Rcpp::as< Rcpp::List >( this_vec );
              write_value( writer, lst, unbox, digits, numeric_dates, factors_as_string, by, row, in_data_frame );
              break;
            }
            default: {
              switch_vector( writer, this_vec, unbox, digits, numeric_dates, factors_as_string, row );
            }
            } // end switch
            
          } // end for
          writer.EndObject();
          
        } else {
          
          writer.StartArray();
          
          for( df_row = 0; df_row < n_rows; df_row++ ) {
            writer.StartObject();
            
            for( df_col = 0; df_col < n_cols; df_col++ ) {
              
              const char *h = column_names[ df_col ];
              writer.String( h );
              SEXP this_vec = df[ h ];
              
              switch( TYPEOF( this_vec ) ) {
              case VECSXP: {
                Rcpp::List lst = Rcpp::as< Rcpp::List >( this_vec );
                write_value( writer, lst, unbox, digits, numeric_dates, factors_as_string, by, df_row, in_data_frame );
                break;
              }
              default: {
                switch_vector( writer, this_vec, unbox, digits, numeric_dates, factors_as_string, df_row );
              }
              }
            }
            writer.EndObject();
          } // end for
          writer.EndArray();
        } // end if
      }
      
    } else {
      
      switch( TYPEOF( list_element ) ) {
      
      case VECSXP: {
     
        // the case where the list item is a row of a data.frame
        // ISSUE #32
        Rcpp::List temp_lst = Rcpp::as< Rcpp::List >( list_element );
        bool has_names;
        
        has_names = temp_lst.hasAttribute("names");
        
        Rcpp::List lst(1);
        if( row >= 0 ) {   // we came in from a data.frame, going by-row
          // ISSUE 32
          lst[0] = temp_lst[ row ];
          
          if( temp_lst.hasAttribute("names") ) {
            Rcpp::CharacterVector templstnames = temp_lst.names();
            const char* this_name = templstnames[ row ];
            lst.names() = this_name;
          }

          write_value( writer, lst, unbox, digits, numeric_dates, factors_as_string, by, -1, in_data_frame );  
          
        } else {
          lst = temp_lst;

          R_xlen_t n = lst.size();
          
          if ( n == 0 ) {
            writer.StartArray();
            writer.EndArray();
            break;
          }
          
          // LIST NAMES
          // issue 53
          Rcpp::CharacterVector list_names( lst.size() ); // initialises an empty-string vector
          has_names = lst.hasAttribute("names");
          
          if ( has_names ) {
            Rcpp::CharacterVector temp_names = lst.names();
            for( i = 0; i < n; ++i ) {
              list_names[i] = temp_names[i] == "" ? list_names[i] : temp_names[i];
            }
          }
          // END LIST NAMES
          
          // issue 44
          // list-column in a data.frame shouldn't be nested inside another array
          jsonify::utils::writer_starter( writer, has_names, in_data_frame );
          
          for ( i = 0; i < n; ++i ) {
            
            SEXP recursive_list = lst[ i ];
            if ( has_names ) {
              const char *s = list_names[ i ];
              writer.String( s );
            }
            // setting in_data_frame to false because we're no longer at the data.frame top-level
            write_value( writer, recursive_list, unbox, digits, numeric_dates, factors_as_string, by, -1, false ); 
          }
          
          jsonify::utils::writer_ender( writer, has_names, in_data_frame );
        } // end if (by row)
        break;
      }
        
      case REALSXP: {
        
        Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( list_element );
        jsonify::writers::simple::write_value( writer, nv, unbox, digits, numeric_dates );
        break;
      }
      case INTSXP: {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( list_element );
        jsonify::writers::simple::write_value( writer, iv, unbox, numeric_dates, factors_as_string );
        break;
      }
      case LGLSXP: {
        Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( list_element );
        jsonify::writers::simple::write_value( writer, lv, unbox );
        break;
      }
      case LISTSXP: {} // lists of dotted paires
      case LANGSXP: {   // language constructs (special lists)
        Rcpp::Pairlist s = Rcpp::as< Rcpp::Pairlist >( list_element );
        Rcpp::List l = Rcpp::as< Rcpp::List >( s );
        write_value( writer, l, unbox, digits, numeric_dates, factors_as_string, by );
        break;
      }
      case CLOSXP: {}   // closures
      case BUILTINSXP: {}
      case SPECIALSXP: {}
      case ENVSXP: {}
      case FUNSXP: {
        Rcpp::List l = Rcpp::as< Rcpp::List >( list_element );
        write_value( writer, l, unbox, digits, numeric_dates, factors_as_string, by );
        break;
      }
      default: {
        Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( list_element );
        jsonify::writers::simple::write_value( writer, sv, unbox );
        break;
      }
      }
    }
  }

} // namespace complex
} // namespace writers
} // namespace jsonify

#endif
