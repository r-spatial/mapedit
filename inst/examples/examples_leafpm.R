library(sf)
library(mapview)

#devtools::install_github("timelyportfolio/mapedit@leafpm")
library(mapedit)
#devtools::install_github("timelyportfolio/leafpm")
library(leafpm)


editFeatures(franconia[1:3,], editor = "leafpm")
