
# This gives an error on CRAN for OSX

# context("test-sf-coercion")

# library(sf)

# p1 <- structure(cbind(0, 0), class = c("XY", "POINT", "sfg"))
# p2 <- structure(cbind(1, 1), class = c("XY", "POINT", "sfg"))
# sf <- structure(data.frame(a = 1:2, geometry = structure(list(p1, p2), class = c("sfc_POINT", "sfc"), 
        # bbox = structure(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), class = "bbox"), 
        # crs = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs"), precision = 0)), 
        # class = c("sf", "data.frame"), sf_column = "geometry", agr = factor(NA, c("constant", "aggregate", "identity")))

# raster_sf <- raster(sf)

# test_that("raster from sf works",
	# { expect_that(raster_sf, is_a("RasterLayer")) } 
# )

# p1 <- rbind(c(-180, -20), c(-140, 55), c(10, 0), c(-140, -60), c(-180, -20))
# hole <- rbind(c(-150, -20), c(-100, -10), c(-110, 20), c(-150, -20))
# p1 <- list(p1, hole)
# p2 <- rbind(c(-10, 0), c(140, 60), c(160, 0), c(140, -55), c(-10, 0))
# p3 <- rbind(c(-125, 0), c(0, 60), c(40, 5), c(15, -45), c(-125, 0))

# pols <- spPolygons(p1, p2, p3)
# sf_pols <- st_as_sf(pols)
# r <- raster(ncol = 90, nrow = 45, vals=1)

# test_that("crop using sfc works",
          # { expect_equal(crop(r, pols), crop(r, sf_pols)) } 
# )

# test_that("mask using sfc works",
          # { expect_equal(mask(r, pols), mask(r, sf_pols)) } 
# )

# test_that("rasterize based on sfc works",
          # { expect_equal(rasterize(pols, r, fun = sum), rasterize(sf_pols, r, fun = sum)) } 
# )

# test_that("extract based on sfc works",
          # { expect_equal(extract(r, pols), extract(r, sf_pols)) } 
# )

