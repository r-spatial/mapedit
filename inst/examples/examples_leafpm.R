library(sf)
library(mapview)
library(mapedit)

#devtools::install_github("timelyportfolio/leafpm")
library(leafpm)


editFeatures(franconia[1:3,], editor = "leafpm")
