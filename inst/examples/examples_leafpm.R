library(sf)
library(mapview)

#devtools::install_github("r-spatial/mapedit@leafpm")
library(mapedit)
#devtools::install_github("r-spatial/leafpm")
library(leafpm)


editFeatures(franconia[1:3,], editor = "leafpm")
