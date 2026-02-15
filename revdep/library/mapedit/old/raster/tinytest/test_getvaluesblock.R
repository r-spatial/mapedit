
#test_that('we can extract from a single layer of a RasterStack using the lyrs argument', {

rast <- raster(matrix(1:16, nrow=4), xmn=0, xmx=4, ymn=0, ymx=4)
  
stk <- stack(list(a=rast, b=sqrt(rast)))
  
expect_equivalent(
    getValuesBlock(stk[[1]], 1, 3, 3, 2, format='m'),
    getValuesBlock(stk, 1, 3, 3, 2, lyrs=1),
)

