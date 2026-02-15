

m <- matrix(c(1L,1L,2L,2L,3L,3L), ncol = 2, byrow = T)
l <- list(m)
res <- geometries:::rcpp_close_matrix(l)
expect_equal( res[[1]], rbind(m, c(1L,1L) ) )

m <- matrix(c(1,1,2,2,3,3), ncol = 2, byrow = T)
l <- list(m)
res <- geometries:::rcpp_close_matrix(l)
expect_equal( res[[1]], rbind(m, c(1,1) ) )

m <- matrix(c(1,1,2,2,3,3), ncol = 2, byrow = T)
l <- list(m)
l <- list(l)
res <- geometries:::rcpp_close_matrix(l)
expect_equal( res[[1]][[1]], rbind(m, c(1,1) ) )

m <- matrix(c(1,1,2,2,3,3), ncol = 2, byrow = T)
l <- list(m)
l <- list(l)
l <- list(l)
res <- geometries:::rcpp_close_matrix(l)
expect_equal( res[[1]][[1]][[1]], rbind(m, c(1,1) ) )

m <- matrix(c(1,1,2,2,3,3), ncol = 2, byrow = T)
df <- as.data.frame( m )
l <- list( df )
expect_error( geometries:::rcpp_close_matrix(l) )

## Too few rows
m <- matrix(c(1,1,2,2), ncol = 2, byrow = T)
l <- list(m)
expect_error( geometries:::rcpp_close_matrix(l), "geometries - closed shapes must have at least 4 rows" )

## already closed
m <- matrix(c(1,1,2,2,3,3,1,1), ncol = 2, byrow = T)
l <- list(m)
res <- geometries:::rcpp_close_matrix(l)
expect_equal( res[[1]], m )

