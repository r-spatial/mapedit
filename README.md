
<!-- README.md is generated from README.Rmd. Please edit that file -->
mapedit
=======

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mapedit)](https://cran.r-project.org/package=mapedit)

Interactive editing of spatial data in R | an [RConsortium](https://www.r-consortium.org/) funded [project](https://www.r-consortium.org/projects/awarded-projects). For additional detail, please see the original [proposal](https://github.com/environmentalinformatics-marburg/mapview_toolchain/blob/master/mapview_interactive_data_manipulation.Rmd).

### Status

`mapedit` is in a very alpha state right now. We would very much appreciate feedback, ideas, and use cases. The API is very likely to change dramatically and rapidly over the next couple of months. We will use semantic versioning to track changes and progress.

### Install

As the CRAN badge above indicates, `mapedit` is a long way from CRAN. To install, please use `devtools`. `mapedit` requires `leaflet.extras` which is not currently on CRAN and which itself requires a recent development version of `leaflet` (&gt;=1.0.2.9008 as of 13 Feb, 2017).

    devtools::install_github("bhaskarvk/leaflet")
    devtools::install_github("bhaskarvk/leaflet.extras")
    devtools::install_github("r-spatial/mapedit")

### Examples

We can interactively CRD (create, update, delete) features on a map with `editMap`.

    library(mapedit)
    library(leaflet)
    library(mapview)

    editMap(leaflet() %>% addTiles())

    editMap(
      mapview(breweries91)@map,
      targetLayerId = "breweries91"
    )

`mapedit` also offers interactive selection of map features with `selectMap`.

    library(mapedit)
    library(leaflet)
    library(mapview)

    selectMap(
      leaflet(breweries91) %>%
        addTiles() %>%
        addCircleMarkers(group = ~brewery)
    )

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
