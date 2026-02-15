

v <- 1L:3L
l <- list(1L,2L,3L)
expect_equal( geometries:::.test_as_list( v ), l )

v <- c(1.1, 2)
l <- list(1.1, 2)
expect_equal( geometries:::.test_as_list( v ), l )

m <- matrix(1L:4L, ncol = 2)
l <- list(1L:2L,3L:4L)
expect_equal( geometries:::.test_as_list( m ), l )

expect_error( geometries:::.test_as_list( c(T) ),  "geometries - unknown object type for converting to list")
