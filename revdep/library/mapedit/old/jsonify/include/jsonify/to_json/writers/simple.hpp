#ifndef R_JSONIFY_WRITERS_SIMPLE_H
#define R_JSONIFY_WRITERS_SIMPLE_H

#include <Rcpp.h>
#include "jsonify/to_json/utils.hpp"
#include "jsonify/to_json/writers/scalars.hpp"

using namespace rapidjson;

namespace jsonify {
namespace writers {
namespace simple {


  // ---------------------------------------------------------------------------
  // vectors
  // ---------------------------------------------------------------------------
  template <typename Writer>
  inline void write_value(
      Writer& writer, 
      Rcpp::StringVector sv, 
      bool unbox
    ) {
    
    R_xlen_t n = sv.size();
    bool will_unbox = jsonify::utils::should_unbox( n, unbox );
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i;

    for ( i = 0; i < n; ++i ) {
      if (Rcpp::StringVector::is_na( sv[i] ) ) {
        writer.Null();
      } else{
        jsonify::writers::scalars::write_value( writer, sv[i] );
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
  /*
   * for writing a single value of a vector
   */
  template <typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::StringVector& sv, 
      int row
  ) {
    
    if ( Rcpp::StringVector::is_na( sv[ row ] ) ) {
      writer.Null();
    } else {
      const char *s = sv[ row ];
      jsonify::writers::scalars::write_value( writer, s );
    }
  }
  
#ifdef LONG_VECTOR_SUPPORT
  /*
   * for writing a single value of a vector
   */
  template <typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::StringVector sv, 
      R_xlen_t& row
    ) {
    
    if ( Rcpp::StringVector::is_na( sv[ row ] ) ) {
      writer.Null();
    } else {
      const char *s = sv[ row ];
      jsonify::writers::scalars::write_value( writer, s );
    }
  }
#endif
  
  template< typename Writer>
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericVector nv, 
      bool unbox, 
      int digits, 
      bool numeric_dates
    ) {

    Rcpp::CharacterVector cls = jsonify::utils::getRClass( nv );
    
    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::date_to_string( nv );
      write_value( writer, sv, unbox );
      
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( nv );
      write_value( writer, sv, unbox );
      
    } else {
    
      R_xlen_t n = nv.size();
      bool will_unbox = jsonify::utils::should_unbox( n, unbox );
      
      jsonify::utils::start_array( writer, will_unbox );
      R_xlen_t i;

      for ( i = 0; i < n; ++i ) {
        if( Rcpp::NumericVector::is_na( nv[i] ) ) {
          writer.Null();
        } else {
          jsonify::writers::scalars::write_value( writer, nv[i], digits );
        }
      }
    jsonify::utils::end_array( writer, will_unbox );
    }
  }
  
  /*
   * For writing a single value of a vector
   */
  template< typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericVector& nv, 
      int row, 
      int digits, 
      bool numeric_dates
  ) {
    
    Rcpp::CharacterVector cls = jsonify::utils::getRClass( nv );
    
    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::date_to_string( nv );
      write_value( writer, sv, row );
      
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( nv );
      write_value( writer, sv, row );
      
    } else {
      if ( Rcpp::NumericVector::is_na( nv[ row ] ) ) {
        writer.Null();
      } else {
        double n = nv[ row ];
        jsonify::writers::scalars::write_value( writer, n, digits );
      }
    }
  }
  
#ifdef LONG_VECTOR_SUPPORT
  /*
   * For writing a single value of a vector
   */
  template< typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericVector nv, 
      R_xlen_t& row, 
      int digits, 
      bool numeric_dates
    ) {

    Rcpp::CharacterVector cls = jsonify::utils::getRClass( nv );
    
    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {

      Rcpp::StringVector sv = jsonify::dates::date_to_string( nv );
      write_value( writer, sv, row );
      
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( nv );
      write_value( writer, sv, row );
      
    } else {
      if ( Rcpp::NumericVector::is_na( nv[ row ] ) ) {
        writer.Null();
      } else {
        double n = nv[ row ];
        jsonify::writers::scalars::write_value( writer, n, digits );
      }
    }
  }
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerVector iv, 
      bool unbox, 
      bool numeric_dates,
      bool factors_as_string
    ) {
    
    Rcpp::CharacterVector cls = jsonify::utils::getRClass( iv );

    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {

      Rcpp::StringVector sv = jsonify::dates::date_to_string( iv );
      write_value( writer, sv, unbox );
      
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( iv );
      write_value( writer, sv, unbox );
      
    } else if ( factors_as_string && Rf_isFactor( iv ) ) {
      
      Rcpp::CharacterVector lvls = iv.attr( "levels" );
      if (lvls.length() == 0 && iv.length() == 0 ) {
        writer.StartArray();
        writer.EndArray();
      } else if ( lvls.length() == 0 ) {
        // no levels - from NA_character_ vector
        Rcpp::StringVector s(1);
        s[0] = NA_STRING;
        R_xlen_t ele = 0;
        write_value( writer, s, ele );
      } else {
        Rcpp::StringVector str = Rcpp::as< Rcpp::StringVector >( iv );
        write_value( writer, str, unbox );
        //write_value( writer, lvls, unbox );
      }
      
    } else {
    
      R_xlen_t n = iv.size();
      bool will_unbox = jsonify::utils::should_unbox( n, unbox );
      jsonify::utils::start_array( writer, will_unbox );
      R_xlen_t i;

      for ( i = 0; i < n; ++i ) {
        if( Rcpp::IntegerVector::is_na( iv[i] ) ) {
          writer.Null();
        } else {
          jsonify::writers::scalars::write_value( writer, iv[i] );
        }
      }
      jsonify::utils::end_array( writer, will_unbox );
    }
  }
  
  /*
   * For writing a single value of a vector
   */
  template< typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerVector& iv, 
      int row, 
      bool numeric_dates, 
      bool factors_as_string
  ) {
    
    Rcpp::CharacterVector cls = jsonify::utils::getRClass( iv );
    
    
    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::date_to_string( iv );
      write_value( writer, sv, row );
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( iv );
      write_value( writer, sv, row );
      
    } else if ( factors_as_string && Rf_isFactor( iv ) ) {
      
      Rcpp::StringVector lvls = iv.attr( "levels" );
      
      if (lvls.length() == 0 && iv.length() == 0 ) {
        writer.StartArray();
        writer.EndArray();
      } else if( lvls.length() == 0 ) {
        // no level s- from NA_character_ vector
        Rcpp::StringVector s(1);
        s[0] = NA_STRING;
        int ele = 0;
        write_value( writer, s, ele );
      } else {
        Rcpp::StringVector str = Rcpp::as< Rcpp::StringVector >( iv );
        write_value( writer, str, row );
      }
      
    } else {
      
      if ( Rcpp::IntegerVector::is_na( iv[ row ] ) ) {
        writer.Null();
      } else {
        int i = iv[ row ];
        jsonify::writers::scalars::write_value( writer, i );
      }
    }
  }
  
#ifdef LONG_VECTOR_SUPPORT
  /*
   * For writing a single value of a vector
   */
  template< typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerVector iv, 
      R_xlen_t& row, 
      bool numeric_dates, 
      bool factors_as_string
    ) {
    
    Rcpp::CharacterVector cls = jsonify::utils::getRClass( iv );
    
    if( !numeric_dates && jsonify::dates::is_in( "Date", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::date_to_string( iv );
      write_value( writer, sv, row );
    } else if ( !numeric_dates && jsonify::dates::is_in( "POSIXt", cls ) ) {
      
      Rcpp::StringVector sv = jsonify::dates::posixct_to_string( iv );
      write_value( writer, sv, row );
      
    } else if ( factors_as_string && Rf_isFactor( iv ) ) {
      
      Rcpp::StringVector lvls = iv.attr( "levels" );
      
      if (lvls.length() == 0 && iv.length() == 0 ) {
        writer.StartArray();
        writer.EndArray();
      } else if( lvls.length() == 0 ) {
        // no level s- from NA_character_ vector
        Rcpp::StringVector s(1);
        s[0] = NA_STRING;
        R_xlen_t ele = 0;
        write_value( writer, s, ele );
      } else {
        Rcpp::StringVector str = Rcpp::as< Rcpp::StringVector >( iv );
        write_value( writer, str, row );
      }
      
    } else {
    
      if ( Rcpp::IntegerVector::is_na( iv[ row ] ) ) {
        writer.Null();
      } else {
        int i = iv[ row ];
        jsonify::writers::scalars::write_value( writer, i );
      }
    }
  }
#endif
  
  template< typename Writer >
  inline void write_value(
      Writer& writer,
      Rcpp::NumericVector nv,
      bool unbox,
      int digits
  ) {
    R_xlen_t n = nv.size();
    bool will_unbox = jsonify::utils::should_unbox( n, unbox );
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i;

    for ( i = 0; i < n; ++i ) {
      if (Rcpp::NumericVector::is_na( nv[i] ) ) {
        writer.Null();
      } else {
        double nn = nv[i];             // required for logical vectors
        jsonify::writers::scalars::write_value( writer, nn, digits );
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
  template< typename Writer >
  inline void write_value(
    Writer& writer,
    Rcpp::IntegerVector iv,
    bool unbox
  ) {
    R_xlen_t n = iv.size();
    bool will_unbox = jsonify::utils::should_unbox( n, unbox );
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i;
    
    for ( i = 0; i < n; ++i ) {
      if (Rcpp::IntegerVector::is_na( iv[i] ) ) {
        writer.Null();
      } else {
        int ii = iv[i];             // required for logical vectors
        jsonify::writers::scalars::write_value( writer, ii );
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
  template <typename Writer>
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalVector lv, 
      bool unbox
    ) {
    
    R_xlen_t n = lv.size();
    bool will_unbox = jsonify::utils::should_unbox( n, unbox );
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i;

    for ( i = 0; i < n; ++i ) {
      if (Rcpp::LogicalVector::is_na( lv[i] ) ) {
        writer.Null();
      } else {
        bool l = lv[i];             // required for logical vectors
        jsonify::writers::scalars::write_value( writer, l );
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalVector& lv, 
      int row
    ) {
    if ( Rcpp::LogicalVector::is_na( lv[ row ] ) ) { 
      writer.Null();
    } else {
      bool l = lv[ row ];
      jsonify::writers::scalars::write_value( writer, l );
    }
  }
  
#ifdef LONG_VECTOR_SUPPORT
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalVector lv, 
      R_xlen_t& row
    ) {
    if ( Rcpp::LogicalVector::is_na( lv[ row ] ) ) { 
      writer.Null();
    } else {
      bool l = lv[ row ];
      jsonify::writers::scalars::write_value( writer, l );
    }
  }
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      SEXP sexp, 
      bool unbox, 
      int digits, 
      bool numeric_dates,
      bool factors_as_string
    ) {
    
    switch( TYPEOF( sexp ) ) {
    case REALSXP: {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sexp );
      write_value( writer, nv, unbox, digits, numeric_dates );
      break;
    }
    case INTSXP: {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( sexp );
      write_value( writer, iv, unbox, numeric_dates, factors_as_string );
      break;
    }
    case LGLSXP: {
      Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( sexp );
      write_value( writer, lv, unbox );
      break;
    }
    default: {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( sexp );
      write_value( writer, sv, unbox );
      break;
    }
    // default: {
    //   Rcpp::stop("Unknown R object type");
    // }
    }
  }
  

  // ---------------------------------------------------------------------------
  // matrix values
  // ---------------------------------------------------------------------------
#ifdef LONG_VECTOR_SUPPORT
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerMatrix mat,
      R_xlen_t& row, 
      bool unbox = false
    ) {
    bool numeric_dates = true;
    bool factors_as_string = true;
    Rcpp::IntegerVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox, numeric_dates, factors_as_string );  // true, true : numeric_dates, factors_as_string
  }
  
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerMatrix& mat,
      int& row, 
      bool unbox = false
  ) {
    
    Rcpp::IntegerVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox, true, true );  // true, true : numeric_dates, factors_as_string
  }
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::IntegerMatrix mat, 
      bool unbox = false,
      std::string by = "row"
  ) {
    
    bool will_unbox = false;
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t n;
    R_xlen_t i;
    bool numeric_dates = true;
    bool factors_as_string = true;
    if ( by == "row" ) {
      n = mat.nrow();
      for ( i = 0; i < n; ++i ) {
        Rcpp::IntegerVector this_row = mat(i, Rcpp::_);
        write_value( writer, this_row, unbox, numeric_dates, factors_as_string );  // true, true : numeric_dates, factors_as_string
      }
    } else { // by == "column"
      n = mat.ncol();
      for( i = 0; i < n; ++i ) {
        Rcpp::IntegerVector this_col = mat( Rcpp::_, i );
        write_value( writer, this_col, unbox, numeric_dates, factors_as_string ); // true, true : numeric_dates, factors_as_string
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
#ifdef LONG_VECTOR_SUPPORT
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericMatrix mat, 
      R_xlen_t& row, 
      bool unbox = false,
      int digits = -1,
      bool numeric_dates = true
    ) {
    // unbox, digits, dates
    // row, digits, dates
    Rcpp::NumericVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox, digits, numeric_dates );
  }
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericMatrix& mat, 
      int& row, 
      bool unbox = false
  ) {
    
    Rcpp::NumericVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox, true, true );  // true, true : numeric_dates, factors_as_string
    
  }
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::NumericMatrix mat, 
      bool unbox = false, 
      int digits = -1, 
      std::string by = "row"
  ) {
    
    bool will_unbox = false;
    jsonify::utils::start_array( writer, will_unbox );
    
    R_xlen_t n;
    R_xlen_t i;
    bool numeric_dates = true;
    
    if ( by == "row" ) {
      n = mat.nrow();
      for ( i = 0; i < n; ++i ) {
        Rcpp::NumericVector this_row = mat(i, Rcpp::_);
        write_value( writer, this_row, unbox, digits, numeric_dates );  // true : numeric dates
      }
    } else { // by == "column"
      n = mat.ncol();
      for( i = 0; i < n; ++i ) {
        Rcpp::NumericVector this_col = mat( Rcpp::_, i );
        write_value( writer, this_col, unbox, digits, numeric_dates );  // true : numeric dates
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }

#ifdef LONG_VECTOR_SUPPORT
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::StringMatrix mat, 
      R_xlen_t& row, 
      bool unbox = false
    ) {

    Rcpp::StringVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox );
  }
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::StringMatrix& mat, 
      int& row, 
      bool unbox = false
  ) {
    
    Rcpp::StringVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox );
  }
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::CharacterMatrix mat, 
      bool unbox = false,
      std::string by = "row"
  ) {
    
    bool will_unbox = false;
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i, n;

    if( by == "row" ) {
      n = mat.nrow();
      for ( i = 0; i < n; ++i ) {
        Rcpp::StringVector this_row = mat( i, Rcpp::_ );
        write_value( writer, this_row, unbox );
      }
    } else { // by == column
      n = mat.ncol();
      for ( i = 0; i < n; ++i ) {
        Rcpp::StringVector this_col = mat( Rcpp::_, i );
        write_value( writer, this_col, unbox );
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
#ifdef LONG_VECTOR_SUPPORT
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalMatrix mat, 
      R_xlen_t& row, 
      bool unbox = false
    ) {
    
    Rcpp::LogicalVector this_row = mat( row, Rcpp::_ );
    write_value( writer, this_row, unbox );
  }
#endif
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalMatrix& mat, 
      int& row, 
      bool unbox = false
  ) {
    
    Rcpp::LogicalVector this_row = mat(row, Rcpp::_);
    write_value( writer, this_row, unbox );
  }
  
  
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      Rcpp::LogicalMatrix mat, 
      bool unbox = false, 
      std::string by = "row"
  ) {
    
    bool will_unbox = false;
    jsonify::utils::start_array( writer, will_unbox );
    R_xlen_t i, n;

    if( by == "row" ) {
      n = mat.nrow();
      
      for ( i = 0; i < n; ++i ) {
        Rcpp::LogicalVector this_row = mat(i, Rcpp::_);
        write_value( writer, this_row, unbox );
      }
    } else { // by == "column;
      n = mat.ncol();
      
      for( i = 0; i < n; ++i ) {
        Rcpp::LogicalVector this_col = mat( Rcpp::_, i );
        write_value( writer, this_col, unbox);
      }
    }
    jsonify::utils::end_array( writer, will_unbox );
  }
  
#ifdef LONG_VECTOR_SUPPORT
  /*
   * template for R SEXPs for single-row from a vector
   */
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      SEXP sexp, 
      R_xlen_t& row, 
      int digits, 
      bool numeric_dates, 
      bool factors_as_string
    ) {
    
    switch( TYPEOF( sexp ) ) {
    case REALSXP: {
    if( Rf_isMatrix( sexp ) ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sexp );
      write_value( writer, nm, row  );
    } else {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sexp );
      write_value( writer, nv, row, digits, numeric_dates );
    }
    break;
    }
    case INTSXP: {
    if( Rf_isMatrix( sexp ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( sexp );
      write_value( writer, im, row );
    } else {
        Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( sexp );
        // TODO( do we need factors_as_string here, or will it be sorted by the time it comes to this step?)
        write_value( writer, iv, row, numeric_dates, factors_as_string );
    }
    break;
    }
    case LGLSXP: {
    if( Rf_isMatrix( sexp ) ) {
      Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( sexp );
      write_value( writer, lm, row );
    } else {
      Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( sexp );
      write_value( writer, lv, row );
    }
    break;
    }
    case STRSXP: {
    if( Rf_isMatrix( sexp ) ) {
      Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( sexp );
      write_value( writer, sm, row );
    } else {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( sexp );
      write_value( writer, sv, row );
    }
    break;
    }
    default: {
      Rcpp::stop("Unknown R object type");
    }
    }
  }
#endif
  
  /*
   * template for R SEXPs for single-row from a vector
   */
  template < typename Writer >
  inline void write_value(
      Writer& writer, 
      SEXP sexp, 
      int row, 
      int digits, 
      bool numeric_dates, 
      bool factors_as_string
  ) {
    
    switch( TYPEOF( sexp ) ) {
    case REALSXP: {
      if( Rf_isMatrix( sexp ) ) {
      Rcpp::NumericMatrix nm = Rcpp::as< Rcpp::NumericMatrix >( sexp );
      write_value( writer, nm, row  );
    } else {
      Rcpp::NumericVector nv = Rcpp::as< Rcpp::NumericVector >( sexp );
      write_value( writer, nv, row, digits, numeric_dates );
    }
    break;
    }
    case INTSXP: {
      if( Rf_isMatrix( sexp ) ) {
      Rcpp::IntegerMatrix im = Rcpp::as< Rcpp::IntegerMatrix >( sexp );
      write_value( writer, im, row );
    } else {
      Rcpp::IntegerVector iv = Rcpp::as< Rcpp::IntegerVector >( sexp );
      // TODO( do we need factors_as_string here, or will it be sorted by the time it comes to this step?)
      write_value( writer, iv, row, numeric_dates, factors_as_string );
    }
    break;
    }
    case LGLSXP: {
      if( Rf_isMatrix( sexp ) ) {
      Rcpp::LogicalMatrix lm = Rcpp::as< Rcpp::LogicalMatrix >( sexp );
      write_value( writer, lm, row );
    } else {
      Rcpp::LogicalVector lv = Rcpp::as< Rcpp::LogicalVector >( sexp );
      write_value( writer, lv, row );
    }
    break;
    }
    case STRSXP: {
      if( Rf_isMatrix( sexp ) ) {
      Rcpp::StringMatrix sm = Rcpp::as< Rcpp::StringMatrix >( sexp );
      write_value( writer, sm, row );
    } else {
      Rcpp::StringVector sv = Rcpp::as< Rcpp::StringVector >( sexp );
      write_value( writer, sv, row );
    }
    break;
    }
    default: {
      Rcpp::stop("Unknown R object type");
    }
    }
  }

} // namespace simple
} // namespace writers
} // namespace jsonify

#endif
