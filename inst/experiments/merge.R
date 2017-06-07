merge_delete <- function(orig = NULL, deletes = NULL, by = c("id" = "layerId")) {
  if(is.null(deletes)) {
    return(orig)
  }

  orig_ids = orig[,names(by)[1], drop = TRUE]
  del_ids = deletes[,by[[1]], drop=TRUE]

  orig[which(!(orig_ids %in% del_ids)),]
}

merge_add <- function(orig = NULL, drawn = NULL) {
  if(is.null(drawn)) {
    return(orig)
  }

  if(is.null(orig)) {
    return(drawn)
  }

  mapedit:::combine_list_of_sf(
    list(orig, drawn)
  )
}

# for reproducibility
#   a sample edit to test_sf
test_sf <- structure(list(feature_type = c("polygon", "rectangle", "rectangle"
), id = c(77L, 89L, 94L), feature = structure(list(structure(list(
structure(c(8.7702, 8.7701, 8.7704, 8.7702, 50.8151, 50.8149,
50.8149, 50.8151), .Dim = c(4L, 2L))), class = c("XY", "POLYGON",
"sfg")), structure(list(structure(c(8.7705, 8.7705, 8.7709, 8.7709,
8.7705, 50.815, 50.8151, 50.8151, 50.815, 50.815), .Dim = c(5L,
2L))), class = c("XY", "POLYGON", "sfg")), structure(list(structure(c(8.7703,
8.7703, 8.7706, 8.7706, 8.7703, 50.8146, 50.8147, 50.8147, 50.8146,
50.8146), .Dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg"
))), n_empty = 0L, class = c("sfc_POLYGON", "sfc"), precision = 0, crs = structure(list(
epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), .Names = c("epsg",
"proj4string"), class = "crs"), bbox = structure(c(8.7701, 50.8146,
8.7709, 50.8151), .Names = c("xmin", "ymin", "xmax", "ymax")))), row.names = c(NA,
3L), class = c("sf", "data.frame"), sf_column = "feature", agr = structure(c(NA_integer_,
NA_integer_), .Names = c("feature_type", "id"), .Label = c("constant",
"aggregate", "identity"), class = "factor"), .Names = c("feature_type",
"id", "feature"))

# how we might delete with editMap
# how we could edit with an id for the feature
#del <- editMap(
#  leaflet() %>% addTiles() %>% addFeatures(test_sf, layerId=~id, group="toedit"),
#  targetLayerId = "toedit"
#)

del <- structure(list(X_leaflet_id = 41L, layerId = 94L, feature = structure(list(
structure(list(structure(c(8.7703, 8.7703, 8.7706, 8.7706,
8.7703, 50.8146, 50.8147, 50.8147, 50.8146, 50.8146), .Dim = c(5L,
2L))), class = c("XY", "POLYGON", "sfg"))), n_empty = 0L, class = c("sfc_POLYGON",
"sfc"), precision = 0, crs = structure(list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), .Names = c("epsg",
"proj4string"), class = "crs"), bbox = structure(c(8.7703, 50.8146,
8.7706, 50.8147), .Names = c("xmin", "ymin", "xmax", "ymax")))), .Names = c("X_leaflet_id",
"layerId", "feature"), row.names = 1L, sf_column = "feature", agr = structure(c(NA_integer_,
NA_integer_), class = "factor", .Label = c("constant", "aggregate",
"identity"), .Names = c("X_leaflet_id", "layerId")), class = c("sf",
"data.frame"))

drwn <- structure(list(X_leaflet_id = c(71L, 90L, 100L, 112L), feature_type = c("polyline",
"polygon", "polygon", "rectangle"), feature = structure(list(
structure(c(8.7711, 8.771, 8.7709, 8.771, 8.7711, 8.7712,
50.8151, 50.815, 50.8149, 50.8149, 50.815, 50.815), .Dim = c(6L,
2L), class = c("XY", "LINESTRING", "sfg")), structure(list(
structure(c(8.7707, 8.7706, 8.7707, 8.7707, 8.7707, 8.7707,
8.7707, 50.8149, 50.8148, 50.8148, 50.8148, 50.8149,
50.8149, 50.8149), .Dim = c(7L, 2L))), class = c("XY",
"POLYGON", "sfg")), structure(list(structure(c(8.7708, 8.7707,
8.7709, 8.7708, 50.8148, 50.8147, 50.8147, 50.8148), .Dim = c(4L,
2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
structure(c(8.7709, 8.7709, 8.771, 8.771, 8.7709, 50.8147,
50.8148, 50.8148, 50.8147, 50.8147), .Dim = c(5L, 2L))), class = c("XY",
"POLYGON", "sfg"))), n_empty = 0L, class = c("sfc_GEOMETRY",
"sfc"), classes = c("LINESTRING", "POLYGON", "POLYGON", "POLYGON"
), precision = 0, crs = structure(list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), .Names = c("epsg",
"proj4string"), class = "crs"), bbox = structure(c(8.7706, 50.8147,
8.7712, 50.8151), .Names = c("xmin", "ymin", "xmax", "ymax")))), .Names = c("X_leaflet_id",
"feature_type", "feature"), row.names = c(NA, 4L), sf_column = "feature", agr = structure(c(NA_integer_,
NA_integer_), class = "factor", .Label = c("constant", "aggregate",
"identity"), .Names = c("X_leaflet_id", "feature_type")), class = c("sf",
"data.frame"))

merge_delete(test_sf, del)
merge_add(test_sf, drwn)
mapview(merge_add(test_sf,drwn))
