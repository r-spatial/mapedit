
df <- data.frame(x = 1:5, y = 1:5)
expect_error( geometries:::.test_column_check( df, 1L:4L ), "geometries - number of columns requested is greater than those available")

expect_silent( geometries:::.test_column_check( 1L, 0L ) )
expect_error( geometries:::.test_column_check( 1L, 1L ), "geometries - invalid geometry column index" )
expect_error( geometries:::.test_column_check( 1L, -1L ), "geometries - invalid geometry column index" )



df <- data.frame(x = 1:5, y = 1:5)
expect_error( geometries:::.test_column_positions(df, 1L), "geometries - expecting string vector of column names when finding column positions")
expect_equal( geometries:::.test_column_positions(df, "x"), 0L )
expect_equal( geometries:::.test_column_positions(df, c("y","x")), c(1L, 0L) )

m <- as.matrix( df )
expect_equal( geometries:::.test_column_positions( m, c("x","y") ), c(0L, 1L) )
expect_equal( geometries:::.test_column_positions( m, c("y", "x") ), c(1L, 0L) )
expect_equal( geometries:::.test_column_positions( m, c("x","z") ), c(0L, -1L) )
expect_error( geometries:::.test_column_positions( m, c(0L, 1L) ), "geometries - expecting string vector of column names when finding column positions" )


expect_equal( geometries:::.test_other_columns( df, NULL, NULL ), c(0,1) )
expect_equal( geometries:::.test_other_columns( df, 1L, NULL ), c(0) )
expect_equal( geometries:::.test_other_columns( df, NULL, 0L ), c(1) )
expect_equal( geometries:::.test_other_columns( df, 1L, 1L ), c(0) )
expect_equal( geometries:::.test_other_columns( df, NULL, 1L ), c(0) )
expect_equal( geometries:::.test_other_columns( df, 0L, NULL ), c(1) )

expect_error( geometries:::.test_other_columns( list(), "x" , NULL ), "geometries - unsupported object" )

m[1,1] <- 1.1
expect_equal( geometries:::.test_other_columns( m, NULL, NULL ), c(0, 1) )
expect_equal( geometries:::.test_other_columns( m, "x", NULL ), c("y") )
expect_equal( geometries:::.test_other_columns( m, 0L, NULL ), c(1) )
