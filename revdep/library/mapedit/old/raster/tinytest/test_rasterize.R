
p1 <- rbind(c(-180, -20), c(-140, 55), c(10, 0), c(-140, -60), c(-180, -20))
hole <- rbind(c(-150, -20), c(-100, -10), c(-110, 20), c(-150, -20))
p1 <- list(p1, hole)
p2 <- rbind(c(-10, 0), c(140, 60), c(160, 0), c(140, -55), c(-10, 0))
p3 <- rbind(c(-125, 0), c(0, 60), c(40, 5), c(15, -45), c(-125, 0))

pols <- spPolygons(p1, p2, p3)
r <- raster(ncol = 90, nrow = 45)
x <- rasterize(pols, r, fun = sum)

# rasterize works as before
expect_equal(sum(values(x), na.rm=TRUE), 3481)



