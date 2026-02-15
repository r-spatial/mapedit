fl <- system.file("external/test.grd", package="raster")
tst <- raster::raster(fl)
raster::crs(tst) <- raster::crs("EPSG:28992")
tf <- tempfile(fileext=".grd")
raster::writeRaster(tst, tf)
tst1 <- raster::raster(tf)
expect_identical(raster::wkt(tst), raster::wkt(tst1))

