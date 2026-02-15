## -----------------------------------------------------------------------------
library(satellite)
path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
sat <- satellite(files)

## -----------------------------------------------------------------------------
sat <- convSC2Rad(sat)
sat <- convSC2Ref(sat)
sat <- convRad2BT(sat)

