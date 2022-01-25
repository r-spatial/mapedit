### these are all copies of unexported functions in mapview

# Initialise mapView map --------------------------------------------------

initMap <- function(map = NULL,
                    map.types = NULL,
                    proj4str,
                    native.crs = FALSE,
                    canvas = FALSE,
                    viewer.suppress = FALSE,
                    platform = mapviewGetOption("platform"),
                    ...) {

  # if (missing(map.types)) map.types <- mapviewGetOption("basemaps")
  ls = list(...)
  nms = names(ls)

  if (platform %in% c("leaflet", "leafgl")) {

    if (is.null(map) & is.null(map.types)) {
      map.types <- mapviewGetOption("basemaps")
    }

    leafletHeight <- mapviewGetOption("leafletHeight")
    leafletWidth <- mapviewGetOption("leafletWidth")

    if (missing(proj4str)) proj4str <- NA
    ## create base map using specified map types
    if (is.null(map)) {
      if (is.na(proj4str) | native.crs) {
        m <- leaflet::leaflet(
          height = leafletHeight,
          width = leafletWidth,
          options = leaflet::leafletOptions(
            minZoom = -1000,
            crs = leafletCRS(crsClass = "L.CRS.Simple"),
            preferCanvas = canvas),
          sizingPolicy = leafletSizingPolicy(
            viewer.suppress = viewer.suppress,
            browser.external = viewer.suppress
          )
        )
      } else {
        m <- initBaseMaps(map.types, canvas = canvas, viewer.suppress = viewer.suppress)
      }
    } else {
      m <- map
    }

  } else if (platform == "mapdeck") {

    map.types = map.types[1]

    if (is.null(map)) {
      if (is.null(map.types)) {
        map.types <- mapviewGetOption("basemaps")[1]
      }

      md_args = try(
        match.arg(
          nms,
          names(as.list(args(mapdeck::mapdeck))),
          several.ok = TRUE
        )
        , silent = TRUE
      )
      if (!inherits(md_args, "try-error")) {
        md_args = ls[md_args]
        md_args$style = map.types
        m = do.call(mapdeck::mapdeck, Filter(Negate(is.null), md_args))
      } else {
        m = mapdeck::mapdeck(style = map.types)
      }
    } else {
      m = map
    }
  }

  return(m)
}
