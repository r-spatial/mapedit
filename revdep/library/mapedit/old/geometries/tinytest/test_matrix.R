
x <- 1:5
df <- data.frame(
  x = 1:5
  , y = 1:5
  , z = 1:5
)
m <- as.matrix( df )
ldf <- as.list( df )

expect_error( geometries:::.test_to_geometry_matrix_cols( list(), 0L ), "geometries - 0-length list found" )
expect_error( geometries:::.test_to_geometry_matrix_cols( list(1), 2L ), "geometries - invalid column index")
expect_error( geometries:::.test_to_geometry_matrix_cols( list(1), c(1L,2L) ), "geometries - number of columns requested is greater than those available")

expect_error( geometries:::.test_to_geometry_matrix_cols( matrix(T), 0L ), "geometries - lines need to be matrices or data.frames" )

expect_error( geometries:::.test_to_geometry_matrix_single( list() ), "geometries - 0-length list found" )
expect_equal( geometries:::.test_to_geometry_matrix_single( x ), matrix( x, ncol = length( x ) ) )
expect_equal( geometries:::.test_to_geometry_matrix_single( c(x, 1.1) ), matrix( c(x, 1.1), ncol = length( x ) + 1 ) )
expect_equal( geometries:::.test_to_geometry_matrix_single( m ), m )

expect_equal( geometries:::.test_to_geometry_matrix_cols( df, NULL ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( df, NULL ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( df, c(0L,1L,2L) ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( df, c(1L,2L) ), unname( m[, 2:3] ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( df, c("x","y","z") ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( df, c("z","x") ), unname( m[, c(3,1)] ) )

expect_error( geometries:::.test_to_geometry_matrix_cols( 1L:5L, c("x") ), "geometries - lines need to be matrices or data.frames" )
expect_error( geometries:::.test_to_geometry_matrix_cols( c(1.1,2), c("x") ), "geometries - lines need to be matrices or data.frames" )

expect_error( geometries:::.test_to_geometry_matrix_cols_names( df, c(0L,1L,2L,3L), FALSE ), "geometries - number of columns requested is greater than those available" )
expect_error( geometries:::.test_to_geometry_matrix_cols_names( df, c(3L), FALSE ), "geometries - invalid column index" )


expect_equal( geometries:::.test_to_geometry_matrix_cols( m, NULL ), m )  ## a named matrix keeps its names
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c(0L,1L,2L) ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c(1L,2L) ), unname( m[, 2:3] ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c("x","y","z") ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c("z","x") ), unname( m[, c(3,1)] ) )



expect_equal( geometries:::.test_to_geometry_matrix_cols( ldf, NULL ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( ldf, c(0L,1L,2L) ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( ldf, c(1L,2L) ), unname( m[, 2:3] ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( ldf, c("x","y","z") ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( ldf, c("z","x") ), unname( m[, c(3,1)] ) )

expect_error( geometries:::.test_to_geometry_matrix_cols( df, list() ), "geometries - unknown column types" )


## make it numeric
m[1,1] <- 1.1
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, NULL ), m )  ## a named matrix keeps its names
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c(0L,1L,2L) ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c(1L,2L) ), unname( m[, 2:3] ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c("x","y","z") ), unname( m ) )
expect_equal( geometries:::.test_to_geometry_matrix_cols( m, c("z","x") ), unname( m[, c(3,1)] ) )
