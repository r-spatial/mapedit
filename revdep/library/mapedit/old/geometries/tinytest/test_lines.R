
line_ids <- c(1,1,1,1,1)
unique_ids <- sort( unique( line_ids ) )
expected <- matrix(c(0,4), ncol = 2)
res <- geometries:::rcpp_id_positions( line_ids, unique_ids )
expect_equal( res, expected )

line_ids <- c(1.1, 1.1)
unique_ids <- sort( unique( line_ids ) )
expected <- matrix(c(0,1), ncol = 2)
res <- geometries:::rcpp_id_positions( line_ids, unique_ids )
expect_equal( res, expected )

line_ids <- c("a")
unique_ids <- sort( unique( line_ids ) )
expected <- matrix(c(0,0), ncol = 2)
res <- geometries:::rcpp_id_positions( line_ids, unique_ids )
expect_equal( res, expected )

line_ids <- c("a","a")
unique_ids <- sort( unique( line_ids ) )
expected <- matrix(c(0,1), ncol = 2)
res <- geometries:::rcpp_id_positions( line_ids, unique_ids )
expect_equal( res, expected )

line_ids <- c(1,1,1,1,2,2,3,3,3,3)
unique_ids <- sort( unique( line_ids ) )
expected <- matrix(c(0,3,4,5,6,9), ncol = 2, byrow = T)
res <- geometries:::rcpp_id_positions( line_ids, unique_ids )
expect_equal( res, expected )

line_ids <- as.integer( c(1,1,1,1,2,2,1,3) )
unique_ids <- as.integer( sort( unique( line_ids ) ) )
expect_error( geometries:::rcpp_id_positions( line_ids, unique_ids ), "geometries - error indexing lines, perhaps caused by un-ordered data?")

line_ids <- c(1.1,1.1,1.1,2,2,1.1,3.1)
unique_ids <- sort( unique( line_ids ) )
expect_error( geometries:::rcpp_id_positions( line_ids, unique_ids ), "geometries - error indexing lines, perhaps caused by un-ordered data?")

