r <- raster::brick(nrows = 100, ncols = 100, nl = 2)
r_z <- setZ(r, as.Date(c("2018-01-01", "2018-01-02")))

s <- stack(r)
s_z <- setZ(s, as.Date(c("2018-01-01", "2018-01-02")))

#test_that("subset keeps the z attribute if present",{
  # on brick
  expect_equal(r_z[[1]]@z$time, as.Date("2018-01-01"))
  expect_equal(s_z[[2]]@z$time, as.Date("2018-01-02"))
  # on stack
  expect_equal(s_z[[c(1,2)]]@z$time, as.Date(c("2018-01-01", "2018-01-02")))
  expect_equal(s_z[[c(1,2)]]@z$time, as.Date(c("2018-01-01", "2018-01-02")))
  # NULL if no z set
  expect_equal(r[[1]]@z$time, NULL)
  expect_equal(s[[1]]@z$time, NULL)


