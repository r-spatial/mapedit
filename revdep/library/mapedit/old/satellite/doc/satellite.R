## -----------------------------------------------------------------------------
library(satellite)
path <- system.file("extdata", package = "satellite") 
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE) # Landsat 8 example data files

sat <- satellite(files)

## -----------------------------------------------------------------------------
str(sat@layers, 1)

## -----------------------------------------------------------------------------
## default (all that are similar to layer 1; panchromatic 15-m band 8 is skipped here)
sat_stack <- stack(sat)
sat_stack

## or by layer names
stack(sat, layer = c("B001n", "B002n", "B003n"))

## or by layer indices
stack(sat, layer = 2:6)

## -----------------------------------------------------------------------------
str(sat@meta)

## -----------------------------------------------------------------------------
## add digital elevation model to existing 'Satellite' object
dem <- raster(system.file("extdata/DEM.TIF", package = "satellite"))
sat <- addSatDataLayer(sat, data = dem, info = NULL, bcde = "DEM", in_bcde = "DEM")

## perform topographic correction
sat_tc <- calcTopoCorr(sat)
tail(sat_tc@meta[, 1:6])

## -----------------------------------------------------------------------------
sat@log

## -----------------------------------------------------------------------------
str(sat_tc@log[1:2])

