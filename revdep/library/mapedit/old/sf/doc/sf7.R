## ----echo=FALSE, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
library(sf)

## -----------------------------------------------------------------------------
library(s2)

## -----------------------------------------------------------------------------
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf")) # wrong ring directions
s2_area(st_as_s2(nc, oriented = FALSE)[1:3]) # corrects ring direction, correct area:
s2_area(st_as_s2(nc, oriented = TRUE)[1:3]) # wrong direction: Earth's surface minus area
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"), check_ring_dir = TRUE)
s2_area(st_as_s2(nc, oriented = TRUE)[1:3]) # no second correction needed here:

## -----------------------------------------------------------------------------
all(units::drop_units(st_area(nc)) == s2_area(st_as_s2(nc, oriented = FALSE)))

## -----------------------------------------------------------------------------
g = st_as_sfc("POLYGON FULL", crs = 'EPSG:4326')
g

## -----------------------------------------------------------------------------
options(s2_oriented = TRUE) # don't change orientation from here on
co = st_as_sf(s2_data_countries())
oc = st_difference(g, st_union(co)) # oceans
b = st_buffer(st_as_sfc("POINT(-30 52)", crs = 'EPSG:4326'), 9800000) # visible half
i = st_intersection(b, oc) # visible ocean
plot(st_transform(i, "+proj=ortho +lat_0=52 +lon_0=-30"), col = 'blue')

## -----------------------------------------------------------------------------
st_area(oc) / st_area(g)

## -----------------------------------------------------------------------------
a = st_as_sfc("POINT(0 0)", crs = 'EPSG:4326')
b = st_as_sfc("POLYGON((0 0,1 0,1 1,0 1,0 0))", crs = 'EPSG:4326')
st_intersects(a, b, model = "open")
st_intersects(a, b, model = "closed")
st_intersects(a, b, model = "semi-open") # a toss
st_intersects(a, b) # default: closed

## -----------------------------------------------------------------------------
fiji = s2_data_countries("Fiji")
aa = s2_data_countries("Antarctica")
s2_bounds_cap(fiji)
s2_bounds_rect(c(fiji,aa))

## -----------------------------------------------------------------------------
sf_use_s2()

## -----------------------------------------------------------------------------
sf_use_s2(FALSE)

## -----------------------------------------------------------------------------
sf_use_s2(TRUE)

## ----eval=require("lwgeom", quietly = TRUE)-----------------------------------
options(s2_oriented = FALSE) # correct orientation from here on
library(sf)
library(units)
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
sf_use_s2(TRUE)
a1 = st_area(nc)
sf_use_s2(FALSE)
a2 = st_area(nc)
plot(a1, a2)
abline(0, 1)
summary((a1 - a2)/a1)

## -----------------------------------------------------------------------------
nc_ls = st_cast(nc, "MULTILINESTRING")
sf_use_s2(TRUE)
l1 = st_length(nc_ls)
sf_use_s2(FALSE)
l2 = st_length(nc_ls)
plot(l1 , l2)
abline(0, 1)
summary((l1 - l2)/l1)

## -----------------------------------------------------------------------------
sf_use_s2(TRUE)
d1 = st_distance(nc, nc[1:10,])
sf_use_s2(FALSE)
d2 = st_distance(nc, nc[1:10,])
plot(as.vector(d1), as.vector(d2))
abline(0, 1)
summary(as.vector(d1) - as.vector(d2))

## -----------------------------------------------------------------------------
sf_use_s2(TRUE)
st_intersects(nc[1:3,], nc[1:3,]) # self-intersections + neighbours
sf_use_s2(TRUE)
st_intersects(nc[1:3,], nc[1:3,], model = "semi-open") # only self-intersections

## ----fig.show='hold', out.width="50%"-----------------------------------------
uk = s2_data_countries("United Kingdom")
class(uk)
uk_sfc = st_as_sfc(uk) 
uk_buffer = st_buffer(uk_sfc, dist = 20000)
uk_buffer2 = st_buffer(uk_sfc, dist = 20000, max_cells = 10000)
uk_buffer3 = st_buffer(uk_sfc, dist = 20000, max_cells = 100)
class(uk_buffer)
plot(uk_sfc)
plot(uk_buffer)
plot(uk_buffer2)
plot(uk_buffer3)
uk_sf = st_as_sf(uk) 

## -----------------------------------------------------------------------------
# the sf way
system.time({
  uk_projected = st_transform(uk_sfc, 27700)
  uk_buffer_sf = st_buffer(uk_projected, dist = 20000)
})
# sf way with few than the 30 segments in the buffer
system.time({
  uk_projected = st_transform(uk_sfc, 27700)
  uk_buffer_sf2 = st_buffer(uk_projected, dist = 20000, nQuadSegs = 4)
})
# s2 with default cell size
system.time({
  uk_buffer = s2_buffer_cells(uk, distance = 20000)
})
# s2 with 10000 cells
system.time({
  uk_buffer2 = s2_buffer_cells(uk, distance = 20000, max_cells = 10000)
})
# s2 with 100 cells
system.time({
  uk_buffer2 = s2_buffer_cells(uk, distance = 20000, max_cells = 100)
})

