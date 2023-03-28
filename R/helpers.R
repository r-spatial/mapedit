utils::globalVariables(c("leaf_id"))

#' @noRd
is_leaflet = function(x) {
  inherits(x, "leaflet")
}

#' @noRd
is_mapview = function(x) {
  inherits(x, "mapview")
}

#' @noRd
is_extent = function(x) {
  inherits(x, "Extent")
}

#' @noRd
is_spatial = function(x) {
  inherits(x, "Spatial")
}

#' @noRd
is_raster = function(x) {
  inherits(x, "Raster")
}

#' @noRd
check_leaflet = function(x, call = caller_env()) {
  rlang::check_required(x, call = call)
  if (is.null(x) || isFALSE(is_leaflet(x))) {
    cli::cli_abort(
      "{.arg x} must be a {.cls leaflet} object.",
      call = call
    )
  }
}

#' @noRd
check_mapview = function(x, call = caller_env()) {
  rlang::check_required(x, call = call)
  if (any(
    c(!is.null(x),
      !is_mapview(x),
      !is_leaflet(x@map)
      ))) {
    cli::cli_abort(
      "{.arg x} must be a {.cls mapview} object.",
      call = call
    )
  }
}

# check for sane longitude bounds of drawn features - latitude is always -90/90
#' @noRd
invalid_longitude_warning = function() {
  cli::cli_warn(
    c("drawn features lie outside standard longitude bounds (-180 to 180)",
      "i" =  "This is likely to cause trouble later!!")
  )
}

### mapview to leaflet
#' @noRd
mapview2leaflet = function(x) {
  methods::slot(x, "map")
}
