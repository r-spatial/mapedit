
#ifndef GEOJSONSF_WKT_H
#define GEOJSONSF_WKT_H

#include "rapidjson/document.h"
using namespace rapidjson;

void coord_separator(std::ostringstream& os, R_xlen_t i, R_xlen_t n);

void begin_wkt(std::ostringstream& os, std::string& geom_type, R_xlen_t& coord_dim );

void end_wkt(std::ostringstream& os, std::string& geom_type);

void point_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

void multi_point_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

void line_string_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

void multi_line_string_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

void polygon_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

void multi_polygon_to_wkt(std::ostringstream& os, const Value& coord_array, R_xlen_t& coord_dim);

#endif

