
l <- geometries:::.tests()

expect_true( is.integer( l$test_bbox$INTSXP ) )
expect_true( !is.integer( l$test_bbox$REALSXP ) )
expect_true( is.numeric( l$test_bbox$REALSXP ) )

# Rcpp::NumericVector x = {1,1,2,2,2,3};
# Rcpp::NumericVector y = {1,1,1,2,2,2};
# Rcpp::NumericVector z = {1,2,3,4,5,6};

expect_equal( l$test_rleid$x, c(1,1,2,2,2,3))
expect_equal( l$test_rleid$y, c(1,1,2,3,3,4))
expect_equal( l$test_rleid$z, c(1,2,3,4,5,6))

expect_equal( l$test_rleid$idx, c(0,2,5))

expect_equal( l$test_matrix$nm1, matrix(c(1:4, 1:4), ncol = 2) )
expect_equal( l$test_matrix$nm2, matrix(c(1:4), ncol = 1) )
expect_equal( l$test_matrix$nm3, matrix(c(1:4), ncol = 1) )
m <- matrix(c(1:4), ncol = 1)
colnames( m ) <- "x"
expect_equal( l$test_matrix$nm4, m)

expect_equal( l$test_matrix$nm5, l$test_matrix$nm1 )
expect_equal( l$test_matrix$nm6, l$test_matrix$nm2 )
expect_equal( l$test_matrix$nm7, l$test_matrix$nm3 )
expect_equal( l$test_matrix$nm8, l$test_matrix$nm4 )

expect_equal( l$test_matrix$nm9, l$test_matrix$nm2 )

expect_equal( colnames( l$test_matrix$nm10 ), c("x","y") )
expect_equal( l$test_matrix$nm11, matrix(1:4, ncol = 4))

expect_equal( l$test_matrix$nm12, l$test_matrix$nm10 )

expect_equal( l$test_matrix$nm13, matrix(1) )

### SEXP col_int
expect_true( l$test_colint$x_col == 0 )
expect_true( l$test_colint$y_col == 1 )



### Other Columns

df <- data.frame(
  x = 1
  , y = 1
  , z = 1
)

expect_equal( geometries:::.test_other_columns( df, 1L, 2L ), 0L )
expect_error( geometries:::.test_other_columns( df, 1, 2 ), "geometries - unsupported column" )
expect_true( is.integer( geometries:::.test_other_columns( df, 1L, 2L ) ) )
# expect_false( is.integer( geometries:::.test_other_columns( df, 1, 2 ) ) )
expect_error( geometries:::.test_other_columns( df, 1, 2L ) , "geometries - different vector types found" )

expect_equal( l$other_col$other_yz_int, c(1L,2L) )
expect_equal( l$other_col$other_zx_int, c(0L,2L) )
expect_equal( l$other_col$other_z_int, c(2L) )

expect_true( is.integer( l$other_col$other_yz_int ) )
expect_true( is.integer( l$other_col$other_zx_int ) )
expect_true( is.integer( l$other_col$other_z_int ) )

# expect_equal( l$other_col$other_yz_dbl, c(1,2) )
# expect_equal( l$other_col$other_zx_dbl, c(0,2) )
# expect_equal( l$other_col$other_z_dbl, c(2) )

# expect_false( is.integer( l$other_col$other_yz_dbl ) )
# expect_false( is.integer( l$other_col$other_zx_dbl ) )
# expect_false( is.integer( l$other_col$other_z_dbl ) )

expect_equal( l$other_col$other_yz_str, c("y","z") )
expect_equal( l$other_col$other_zx_str, c("x","z") )
expect_equal( l$other_col$other_z_str, c("z") )

## as_list.hpp
expect_true( all( l$test_list$list_mat[[1]] == 1:4 ) )
expect_true( all( l$test_list$list_mat[[2]] == 4:1 ) )

## a data.frame converted to a list remains unchanged
expect_true( is.data.frame( l$test_list$list_df ) )

## unnamed-list
expect_true( is.null( names( l$test_list$list_list_mat ) ) )
expect_true( all( names( l$test_list$list_df ) == c("x","y") ) )

## fill_list.hpp
expect_equal( l$test_fill_list[[1]], c(1,2) )
expect_equal( l$test_fill_list[[2]], c(3,4) )


