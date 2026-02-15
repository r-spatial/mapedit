## -----------------------------------------------------------------------------
library(sp)
demo(meuse, ask = FALSE, echo = FALSE) # loads the meuse data sets
class(meuse)

## -----------------------------------------------------------------------------
library(sf)
nc <- as(st_read(system.file("gpkg/nc.gpkg", package="sf")), "Spatial")

## -----------------------------------------------------------------------------
plot(meuse)

## -----------------------------------------------------------------------------
plot(meuse, pch = 1, cex = sqrt(meuse$zinc)/12, axes = TRUE)
v = c(100,200,400,800,1600)
legend("topleft", legend = v, pch = 1, pt.cex = sqrt(v)/12)
plot(meuse.riv, add = TRUE, col = grey(.9, alpha = .5))

## -----------------------------------------------------------------------------
proj4string(meuse)

## ----fig.height=8-------------------------------------------------------------
crs.longlat = CRS("+init=epsg:4326")
meuse.longlat = spTransform(meuse, crs.longlat)
plot(meuse.longlat, axes = TRUE)

## -----------------------------------------------------------------------------
par(mar = rep(0,4))
plot(meuse.longlat, asp = 1)

## ----fig.height=7.5-----------------------------------------------------------
par(mar = c(0, 0, 1, 0))
library(methods) # as
plot(as(meuse, "Spatial"), expandBB = c(.05, 0, 0, 0))
plot(gridlines(meuse), add = TRUE, col = grey(.8))
plot(meuse, add = TRUE)
text(labels(gridlines(meuse)), col = grey(.7))
title("default gridlines with Meuse projected data")

## -----------------------------------------------------------------------------
par(mar = c(0, 0, 1, 0))
grd <- gridlines(meuse.longlat)
grd_x <- spTransform(grd, CRS(proj4string(meuse)))
plot(as(meuse, "Spatial"), expandBB = c(.05, 0, 0, 0))
plot(grd_x, add=TRUE, col = grey(.8))
plot(meuse, add = TRUE)
text(labels(grd_x, crs.longlat), col = grey(.7))
title("longitude latitude gridlines and labels")

## -----------------------------------------------------------------------------
# demonstrate axis labels with angle, both sides:

maps2sp = function(xlim, ylim, l.out = 100, clip = TRUE) {
	stopifnot(require(maps))
	m = map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
	as(st_as_sf(m), "Spatial")
}
par(mar = c(0, 0, 1, 0))
m = maps2sp(c(-100,-20), c(10,55))
sp = SpatialPoints(rbind(c(-101,9), c(-101,55), c(-19,9), c(-19,55)), CRS("+init=epsg:4326"))
laea = CRS("+proj=laea +lat_0=30 +lon_0=-40")
m.laea = spTransform(m, laea)
sp.laea = spTransform(sp, laea)
plot(as(m.laea, "Spatial"), expandBB = c(.1, 0.05, .1, .1))
plot(m.laea, col = grey(.8), add = TRUE)
gl = gridlines(sp, easts = c(-100,-80,-60,-40,-20), norths = c(20,30,40,50))
gl.laea = spTransform(gl, laea)
plot(gl.laea, add = TRUE)
text(labels(gl.laea, crs.longlat))
text(labels(gl.laea, crs.longlat, side = 3:4), col = 'red')
title("curved text label demo")

# polar:
par(mar = c(0, 0, 1, 0))
pts=SpatialPoints(rbind(c(-180,-70),c(0,-70),c(180,-89),c(180,-70)), CRS("+init=epsg:4326"))
gl = gridlines(pts, easts = seq(-180,180,20), ndiscr = 100)
polar = CRS("+init=epsg:3031")
plot(spTransform(pts, polar), expandBB = c(.05, 0, .05, 0))
gl.polar = spTransform(gl, polar)
lines(gl.polar)
l = labels(gl.polar, crs.longlat, side = 3)
l$pos = NULL # pos is too simple, use adj:
text(l, adj = c(0.5, -0.5), cex = .8) 
l = labels(gl.polar, crs.longlat, side = 4)
l$srt = 0 # otherwise they end up upside-down
text(l, cex = .8)
title("grid line labels on polar projection, epsg 3031")

par(mar = c(0, 0, 1, 0))
m = maps2sp(xlim = c(-180,180), ylim = c(-90,-70), clip = FALSE)
gl = gridlines(m, easts = seq(-180,180,20))
polar = CRS("+init=epsg:3031")
gl.polar = spTransform(gl, polar)
plot(as(gl.polar, "Spatial"), expandBB = c(.05, 0, .05, 0))
plot(gl.polar, add = TRUE)
plot(spTransform(m, polar), add = TRUE, col = grey(0.8, 0.8))
l = labels(gl.polar, crs.longlat, side = 3)
# pos is too simple here, use adj:
l$pos = NULL 
text(l, adj = c(0.5, -0.3), cex = .8)
l = labels(gl.polar, crs.longlat, side = 2)
l$srt = 0 # otherwise they are upside-down
text(l, cex = .8)
title("grid line labels on polar projection, epsg 3031")


## -----------------------------------------------------------------------------
par(mar = c(0, 0, 1, 0))
plot(nc)
invisible(text(coordinates(nc), labels=as.character(nc$NAME), cex=0.4))

## -----------------------------------------------------------------------------
names(nc)
rrt <- nc$SID74/nc$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
par(mar = c(0, 0, 1, 0))
plot(nc, col=cols[findInterval(rrt, brks, all.inside=TRUE)])

## -----------------------------------------------------------------------------
rrt <- nc$SID74/nc$BIR74
brks <- quantile(rrt, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
par(mar = rep(0,4))
plot(nc, density=dens[findInterval(rrt, brks, all.inside=TRUE)])

## -----------------------------------------------------------------------------
image(meuse.grid)

## -----------------------------------------------------------------------------
plot(meuse.grid["dist"])
points(meuse, col = 'green')

## -----------------------------------------------------------------------------
plot(meuse.grid["dist"], zlim = c(0,1))
plot(geometry(meuse.grid), add = TRUE, col = grey(.8))

## -----------------------------------------------------------------------------
spplot(meuse, "zinc", do.log = TRUE,
	key.space=list(x = 0.1, y = 0.95, corner = c(0, 1)),
	scales=list(draw = TRUE))

## -----------------------------------------------------------------------------
spplot(meuse, "zinc", do.log = TRUE,
	key.space=list(x=0.2,y=0.9,corner=c(0,1)),
	scales=list(draw = TRUE), cuts = 3,
	legendEntries = c("low", "intermediate", "high"))

## -----------------------------------------------------------------------------
scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
	offset = c(178600,332490), scale = 500, fill=c("transparent","black"))
text1 = list("sp.text", c(178600,332590), "0")
text2 = list("sp.text", c(179100,332590), "500 m")
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
	offset = c(178750,332000), scale = 400)
spplot(meuse, "zinc", do.log=T,
	key.space=list(x=0.1,y=0.93,corner=c(0,1)),
	sp.layout=list(scale,text1,text2,arrow),
	main = "Zinc (top soil)")

## -----------------------------------------------------------------------------
rv = list("sp.polygons", meuse.riv, fill = "lightblue")
scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
	offset = c(180500,329800), scale = 500, fill=c("transparent","black"), which = 1)
text1 = list("sp.text", c(180500,329900), "0", which = 1)
text2 = list("sp.text", c(181000,329900), "500 m", which = 1)
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
	offset = c(178750,332500), scale = 400)
spplot(meuse["zinc"], do.log = TRUE,
	key.space = "bottom", 
	sp.layout = list(rv, scale, text1, text2),
	main = "Zinc (top soil)",
	legend = list(right = list(fun = mapLegendGrob(layout.north.arrow()))))

## -----------------------------------------------------------------------------
spplot(meuse, "zinc", panel = function(x, y, ...) {
		sp.polygons(meuse.riv, fill = "lightblue")
		SpatialPolygonsRescale(layout.scale.bar(), offset = c(179900,329600), 
			scale = 500, fill=c("transparent","black"))
		sp.text(c(179900,329700), "0")
		sp.text(c(180400,329700), "500 m")
		SpatialPolygonsRescale(layout.north.arrow(), 
			offset = c(178750,332500), scale = 400)
		panel.pointsplot(x, y, ...)
	},
	do.log = TRUE, cuts = 7,
	key.space = list(x = 0.1, y = 0.93, corner = c(0,1)),
	main = "Top soil zinc concentration (ppm)")

## -----------------------------------------------------------------------------
rv = list("sp.polygons", meuse.riv, fill = "lightblue")
scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
	offset = c(180500,329800), scale = 500, fill=c("transparent","black"), which = 4)
text1 = list("sp.text", c(180500,329900), "0", cex = .5, which = 4)
text2 = list("sp.text", c(181000,329900), "500 m", cex = .5, which = 4)
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
	offset = c(181300,329800), 
	scale = 400, which = 4)
cuts = c(.2,.5,1,2,5,10,20,50,100,200,500,1000,2000)
spplot(meuse, c("cadmium", "copper", "lead", "zinc"), do.log = TRUE,
	key.space = "right", as.table = TRUE,
	sp.layout=list(rv, scale, text1, text2, arrow), # note that rv is up front!
	main = "Heavy metals (top soil), ppm", cex = .7, cuts = cuts)

## -----------------------------------------------------------------------------
rv = list("sp.polygons", meuse.riv, fill = "blue", alpha = 0.1)
pts = list("sp.points", meuse, pch = 3, col = "grey", alpha = .5)
text1 = list("sp.text", c(180500,329900), "0", cex = .5, which = 4)
text2 = list("sp.text", c(181000,329900), "500 m", cex = .5, which = 4)
scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
	offset = c(180500,329800), scale = 500, fill=c("transparent","black"), which = 4)
library(gstat)
v.ok = variogram(log(zinc)~1, meuse)
ok.model = fit.variogram(v.ok, vgm(1, "Exp", 500, 1))
# plot(v.ok, ok.model, main = "ordinary kriging")
v.uk = variogram(log(zinc)~sqrt(dist), meuse)
uk.model = fit.variogram(v.uk, vgm(1, "Exp", 300, 1))
# plot(v.uk, uk.model, main = "universal kriging")
meuse[["ff"]] = factor(meuse[["ffreq"]])
meuse.grid[["ff"]] = factor(meuse.grid[["ffreq"]])
v.sk = variogram(log(zinc)~ff, meuse)
sk.model = fit.variogram(v.sk, vgm(1, "Exp", 300, 1))
# plot(v.sk, sk.model, main = "stratified kriging")
zn.ok = krige(log(zinc)~1,          meuse, meuse.grid, model = ok.model, debug.level = 0)
zn.uk = krige(log(zinc)~sqrt(dist), meuse, meuse.grid, model = uk.model, debug.level = 0)
zn.sk = krige(log(zinc)~ff,         meuse, meuse.grid, model = sk.model, debug.level = 0)
zn.id = krige(log(zinc)~1,          meuse, meuse.grid, debug.level = 0)

zn = zn.ok
zn[["a"]] = zn.ok[["var1.pred"]]
zn[["b"]] = zn.uk[["var1.pred"]]
zn[["c"]] = zn.sk[["var1.pred"]]
zn[["d"]] = zn.id[["var1.pred"]]
spplot(zn, c("a", "b", "c", "d"), 
	names.attr = c("ordinary kriging", "universal kriging with dist to river", 
		"stratified kriging with flood freq", "inverse distance"), 
	as.table = TRUE, main = "log-zinc interpolation",
	sp.layout = list(rv, scale, text1, text2)
)

## -----------------------------------------------------------------------------
rv = list("sp.polygons", meuse.riv, fill = "blue", alpha = 0.1)
pts = list("sp.points", meuse, pch = 3, col = "grey", alpha = .7)
spplot(zn.uk, "var1.pred",
	sp.layout = list(rv, scale, text1, text2, pts),
	main = "log(zinc); universal kriging using sqrt(dist to Meuse)")
zn.uk[["se"]] = sqrt(zn.uk[["var1.var"]])
spplot(zn.uk, "se", sp.layout = list(rv, pts),
	main = "log(zinc); universal kriging standard errors")

## -----------------------------------------------------------------------------
arrow = list("SpatialPolygonsRescale", layout.north.arrow(),
    offset = c(-76,34), scale = 0.5, which = 2)
spplot(nc, c("SID74", "SID79"), names.attr = c("1974","1979"),
    colorkey=list(space="bottom"), scales = list(draw = TRUE),
    main = "SIDS (sudden infant death syndrome) in North Carolina",
    sp.layout = list(arrow), as.table = TRUE)

## -----------------------------------------------------------------------------
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
	offset = c(-76,34), scale = 0.5, which = 2)
#scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
#	offset = c(-77.5,34), scale = 1, fill=c("transparent","black"), which = 2)
#text1 = list("sp.text", c(-77.5,34.15), "0", which = 2)
#text2 = list("sp.text", c(-76.5,34.15), "1 degree", which = 2)
# create a fake lines data set:
## multi-panel plot with coloured lines: North Carolina SIDS
spplot(nc, c("SID74","SID79"), names.attr = c("1974","1979"),
    colorkey=list(space="bottom"),
    main = "SIDS (sudden infant death syndrome) in North Carolina",
    sp.layout = arrow, as.table = TRUE)

## -----------------------------------------------------------------------------
b1 = bubble(meuse, "cadmium", maxsize = 1.5, main = "cadmium concentrations (ppm)",
	key.entries = 2^(-1:4))
b2 = bubble(meuse, "zinc", maxsize = 1.5, main = "zinc concentrations (ppm)",
	key.entries =  100 * 2^(0:4))
print(b1, split = c(1,1,2,1), more = TRUE)
print(b2, split = c(2,1,2,1), more = FALSE)

## -----------------------------------------------------------------------------
# create two dummy factor variables, with equal labels:
set.seed(31)
nc$f = factor(sample(1:5, 100,replace = TRUE),labels=letters[1:5])
nc$g = factor(sample(1:5, 100,replace = TRUE),labels=letters[1:5])
library(RColorBrewer)
## Two (dummy) factor variables shown with qualitative colour ramp; degrees in axes
spplot(nc, c("f","g"), col.regions=brewer.pal(5, "Set3"), scales=list(draw = TRUE))


## ----results="markup"---------------------------------------------------------
library(mapview)
mapview(meuse, zcol = c("zinc", "lead"), legend = TRUE)

## ----results="markup"---------------------------------------------------------
mapview(meuse.grid, zcol = c("soil", "dist"), legend = TRUE)

## ----results="markup"---------------------------------------------------------
mapview(nc, zcol = c("SID74", "SID79"), alpha.regions = 1.0, legend = TRUE)

## ----results="markup",eval=FALSE----------------------------------------------
# m1 <- mapview(meuse, zcol = "soil", burst = TRUE, legend = TRUE)
# m2 <- mapview(meuse, zcol = "lead", legend = TRUE)
# m3 <- mapview(meuse, zcol = "landuse", map.types = "Esri.WorldImagery", legend = TRUE)
# m4 <- mapview(meuse, zcol = "dist.m", legend = TRUE)
# sync(m1, m2, m3, m4) # 4 panels synchronised
# # latticeView(m1, m2, m3, m4) # 4 panels

## -----------------------------------------------------------------------------
sessionInfo()

