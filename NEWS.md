# mapedit 0.7.0.9001 (2025-04-21)

#### ✨ features and improvements

#### 🐛 bug fixes

#### 💬 documentation etc

#### 🍬 miscellaneous


# mapedit 0.7.0 (2025-04-20)

#### ✨ features and improvements

* add editAttributes app and RStudio addin #120

#### 🐛 bug fixes

* update deprecated dplyr::select_() calls #124


# mapedit 0.6.2 (2025-04-19)

### Bug Fix

* properly flush deleted features when user deletes multiple. See [issue #106](https://github.com/r-spatial/mapedit/issues/106).

* allow `mapedit` to work in a loop or in sequence. See [issue #83](https://github.com/r-spatial/mapedit/issues/83).

* make behavior of `selectFeatures` consistent between modes `"draw"` and `"click"` when working with features with no CRS.  See [issue #110](https://github.com/r-spatial/mapedit/issues/110).

### New Features

* Add listener for all features and add to returned features with `leaflet.extras` editor.  See [pull #98](https://github.com/r-spatial/mapedit/pull/98).

* Add `editorOptions` for better control of options in `edit*` and `draw*` functions.  See [pull #100](https://github.com/r-spatial/mapedit/pull/100) and [pull #103](https://github.com/r-spatial/mapedit/pull/103).

### New Features

* add `editor  = "leafpm"` to `draw*()` and `edit*()` functions to use the `Leaflet.pm` pluging for editing.  `Leaflet.pm` provides support for creating and editing holes, snapping, and integrates better with some `multi*` features.  Note, `mapedit` now offers two editors `"leaflet.extras"` and `"leafpm"`, since each have advantages and disadvantages.

### New Features

* stopApp when session ended to handle when a user closes a browser tab or window when using `viewer = browserViewer(browser = getOption("browser"))`

* add circleMarkerOptions for Leaflet.draw

* warn when feature drawn outside of -180, 180

### Bug Fixes

* fix precision to match new Leaflet 6 digits

* expose title argument to editFeatures and drawFeatures

### Bug Fix

* fix `edit` module lookup for already added `Leaflet.draw`

### New Features

* add drawing mode to selectFeatures to enable selection via point/line/polygon drawing. Selection will be done using any of `?geos_binary_ops`; thx @tim-salabim

* add `CRS` in `edit*` functions; thx @tim-salabim

* add label for reference in `edit*` and `select*`

* add title argument for `editMap()`

* automatically close browser window on Shiny session end when using
`viewer = browserViewer(browser = getOption("browser"))`

* add new function drawFeatures


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
