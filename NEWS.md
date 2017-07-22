# mapedit 0.3.3

### New Features

* add `CRS` in `edit*` functions; thx @tim-salabim

* add label for reference in `edit*` and `select*`

# mapedit 0.3.2

### Bug Fix

* polygons of `length > 1` not handled correctly.  See [discussion](https://github.com/r-spatial/mapedit/issues/48).

* remove internally added `edit_id` column in editFeatures return

* cast edits back to their original type.  See [discussion](https://github.com/r-spatial/mapedit/issues/48)

* fix merge_edit to only consider last edit when there are multiple edits per layerId

# mapedit 0.3.1

### Bug Fix

* multiple edits and deletes resulting in multiple FeatureCollections not handled properly causing some actions to not be considered when converting to `sf`; thanks @tim-salabim for identifying

# mapedit 0.3.0

### API Changes

* **BREAKING** editFeatures and selectFeatures add a map argument
instead of platform

### New Features

* add editFeatures function for easy add, edit, delete with existing simple features (sf)
* add editMap.NULL
* add record argument to edit* functions to preserve the series
of actions from an editing session.  If `record = TRUE` then a `"recorder"` attribute will be added to the returned object for
full reproducibility.
* add internal playback for recorded session for future use
* add viewer argument to select and edit functions to allow
user the flexibility to adjust the viewer experience.  Default
will be paneViewer() in an attempt to keep the workflow
within one RStudio window/context.
* change height to 97% to fill viewer
* document more thoroughly
* pass trial CRAN check

## Bug Fixes

* fix internal `combine_list_of_sf` with length 0 `list`; found when `editFeatures()` and save with no changes

# mapedit 0.2.0

* add Shiny module functionality
* add selectFeatures function for easy selection of features from simple features (sf)
* defaults to repeat mode in editMap()
* removes circle Leaflet.draw tool by default in editMap()
* use layerId instead of group for select
* uses Viewer window for selectMap()
* promote mapview to Imports
* uses newly exported mapview::addFeatures()


# mapedit 0.1.0

**API breaking change**

* camelCase `editMap` and `selectMap`

# mapedit 0.0.2

* add dependency on `dplyr`
* add dependency on `sf`
* `edit_map()` now returns `sf` instead of `geojson` by default.  Toggle
    behavior with the `sf` argument.

# mapedit 0.0.1

* first release with proof-of-concept functionality
