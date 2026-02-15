# version 0.2-15

* `st_geod_azimuth()` adds feature/pairwise azimuth #97 by @robitalec 

* add `st_geom_from_geohash()`; #37

* fix default for argument `digits` in `st_astext()`

# version 0.2-14

* `st_perimeter()` is deprecated in favor of `st_perimeter_lwgeom()`, as `sf` takes over with `sf::st_perimeter()`.

# version 0.2-13

# version 0.2-11

* replace `sprintf()` instances with `snprintf()`

# version 0.2-10

* fix -Wstrict-prototypes warnings

# version 0.2-9

* fix formatting issues for long long int

# version 0.2-8

* remove PROBLEM ... ERROR constructs from C code

# version 0.2-5

* GEOS requirement lowered to 3.5.0, which also seems to work; #59.

# version 0.2-4

* require sf >= 0.9-3, and use C API PROJ path setting (to work on CRAN windows binaries)

* update to new GEOS (3.8.0) and PROJ (6.3.1) versions for CRAN windows binary builds

* require GEOS 3.6.0 (required by PostGIS 3.0.0), and add check to configure

# version 0.2-3

* fix configure script to work with ubuntu/bionic and PROJ 4.9.3; #28

* fix configure script to work with PROJ 5.x versions

# version 0.2-2

* adjust to sf >= 0.9-0 new crs representation

* use `st_make_valid()` generic from package sf; https://github.com/r-spatial/sf/issues/1300

# version 0.2-1

* fix PROJ 5.x installation issue (has proj.h, but shouldn't use it)

# version 0.2-0

* export `lwgeom_make_valid()`, to gradually move `st_make_valid()` from `lwgeom` to `sf`; https://github.com/r-spatial/sf/issues/989

* constrain argument `crs` in `st_transform_proj()` to take one or two character strings

* update to POSTGIS 3.0.0 liblwgeom version

* update to modern PROJ, use proj.h when available

# version 0.1-5

* check for user interrupts on `st_geod_distance()`, #29 by Dan Baston

* add `st_astext()` for fast WKT writing, #25 by Etienne Racine

* add `st_is_polygon_cw()`, #21 by Andy Teucher @ateucher; add Andy Teucher to contributors

* add `st_perimeter()` and `st_perimeter_2d()` functions to compute the length measurement of the boundary of a surface.

* allow `st_transform_proj()` to take two proj4 strings as crs, as `c(input_p4s, output_p4s)`, ignoring the CRS of x

# version 0.1-4

* tries to fix the CRAN error for r-release-osx (datum files missing in sf; removed test)

# version 0.1-3

* add `st_geod_covered_by()` binary geometry predicate

# version 0.1-2

* try to fix OSX compile on CRAN, tuning configure.ac

# version 0.1-1

* add `st_length()`

* attempt to fix Solaris and OSX

* report proj.4 and GEOS versions on startup, and on `lwgeom_extSoftwareVersions`; #10

* add minimum bounding circle, by @rundel; #7

* add `st_subdivide()`, see https://github.com/r-spatial/sf/issues/597

# version 0.1-0

* first CRAN submission
