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
        m <- initBaseMaps(
          map.types
          , canvas = canvas
          , viewer.suppress = viewer.suppress
        )
      }
    } else {
      m <- map
    }

  }

  return(m)
}


initBaseMaps <- function(map.types, canvas = FALSE, viewer.suppress = FALSE) {
  ## create base map using specified map types
  if (missing(map.types)) map.types <- mapviewGetOption("basemaps")
  leafletHeight <- mapviewGetOption("leafletHeight")
  leafletWidth <- mapviewGetOption("leafletWidth")
  lid <- 1:length(map.types)
  m <- leaflet::leaflet(
    height = leafletHeight,
    width = leafletWidth,
    options = leaflet::leafletOptions(
      minZoom = 1,
      maxZoom = 52,
      bounceAtZoomLimits = FALSE,
      maxBounds = list(
        list(c(-90, -370)),
        list(c(90, 370))),
      preferCanvas = canvas),
    sizingPolicy = leafletSizingPolicy(
      viewer.suppress = viewer.suppress,
      browser.external = viewer.suppress
    )
  )
  if (!(is.null(map.types))) {
    m <- leaflet::addProviderTiles(m, provider = map.types[1],
                                   layerId = map.types[1], group = map.types[1],
                                   options = providerTileOptions(
                                     pane = "tilePane"
                                   ))
    if (length(map.types) > 1) {
      for (i in 2:length(map.types)) {
        m <- leaflet::addProviderTiles(m, provider = map.types[i],
                                       layerId = map.types[i], group = map.types[i],
                                       options = providerTileOptions(
                                         pane = "tilePane"
                                       ))
        m = removeDuplicatedMapDependencies(m)
      }
    }
  }
  return(m)
}

removeDuplicatedMapDependencies <- function(map) {
  ind <- duplicated(map$dependencies)
  if (any(ind)) map$dependencies[ind] <- NULL
  return(map)
}
