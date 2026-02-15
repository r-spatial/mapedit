# v0.4.5

* Checks for missing Z and M values [issue 100](https://github.com/dcooley/sfheaders/issues/100)

# v0.4.4

* bumped {geometries} dependency

# v0.4.3

* removed C++ system requirements

# v0.4.2

* updated C++ dependencies

# v0.4

* list-column properties are better supported for nested geometries (MULTILINESTRING, POLYGON, MULTIPOLYGON)

# v0.3.1

* `z_range` and `m_range` only created if those dimensions exist

# v0.3.0

* changed licence to MIT
* Faster sf object creation by Linking-To [geometries](https://github.com/dcooley/geometries) library

# v0.2.2

* crs structure matches new `sf` definition [issue49](https://github.com/dcooley/sfheaders/issues/49)
* `sf_to_df()` adds `sfc_columns` attributes to identify which of the columns form the coordinates [issue50](https://github.com/dcooley/sfheaders/issues/50)
* `XYM` dimensions correctly handled
* `unlist` argument in `sf_to_df()` to specify columns you want to unlist when converted to data.frame
* helper functions with easier syntax - thanks to @mdsumner
* `list_columns` argument in `sf_` functions to specify which columns should be filled with all the values in a list

# v 0.2.1

* `sf_remove_holes()` removes holes from polygons
* `calculate_bbox()` function for (re) calculating the bounding box of an `sfc` object
* `sf_cast()` functions for casting between geometry types
* optimised converting `sfc_POINT`-only objects to data.frames
* `sf_to_df()` adds `sfc_columns` attributes to identify which of the columns form the coordinates

# v 0.1

* `keep = TRUE` argument to keep other data columns when converting to `sf`
* `sf_to_df` functions for converting sf objects to data.frames
* `fill = TRUE` argument to fill the data.frame columns with data
