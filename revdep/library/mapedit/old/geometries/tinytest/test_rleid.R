
df <- data.frame(
  x = c(1,1,2,2,3)
  , y = c("a","a","a","b","a")
  , z = I(list(1,2,3,4,5))
  , a = c(T,T,T,T,T)
)


expect_equal( geometries:::rcpp_rleid( df, c(0L)), c(1,1,2,2,3) )
expect_equal( geometries:::rcpp_rleid( df, c(3L)), c(1,1,1,1,1) )
expect_equal( geometries:::rcpp_rleid( df, c(1L)), c(1,1,1,2,3) )
expect_equal( geometries:::rcpp_rleid( df, c(0L,1L)), c(1,1,2,3,4) )
expect_error( geometries:::rcpp_rleid( df, 2L), "geometries - unsupported id column type" )

expect_equal( geometries:::rcpp_rleid_indices( df$x ), c(0,2,4) )
expect_equal( geometries:::rcpp_rleid_indices( df$y ), c(0,3,4) )
expect_error( geometries:::rcpp_rleid_indices( df$z ), "geometries - unsupported vector type" )
