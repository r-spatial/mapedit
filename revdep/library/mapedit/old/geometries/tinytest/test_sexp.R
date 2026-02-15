
df <- data.frame(
  x = 1L
  , y = 1L
)

mint <- as.matrix( df )

df <- data.frame(
  x = 1.1
  , y = 1
)

mdbl <- as.matrix( df )
lst <- as.list( df )

expect_error( geometries:::.test_sexp_col_int( df, 1 ), "geometries - require either integer or string column indices" )
expect_equal( geometries:::.test_sexp_col_int( df, 0L ), 0 )
expect_equal( geometries:::.test_sexp_col_int( df, 1L ), 1 )
expect_equal( geometries:::.test_sexp_col_int( df, "x" ), 0 )
expect_equal( geometries:::.test_sexp_col_int( df, "y" ), 1 )

expect_equal( geometries:::.test_sexp_col_names( df ), names( df ) )
expect_equal( geometries:::.test_sexp_col_names( mint ), names( df ) )
expect_equal( geometries:::.test_sexp_col_names( mdbl ), names( df ) )
expect_equal( geometries:::.test_sexp_col_names( lst ), names( df ) )
expect_error( geometries:::.test_sexp_col_names( 1:4 ), "geometries - object does not have names" )

expect_equal( geometries:::.test_sexp_n_col( df ), 2 )
expect_equal( geometries:::.test_sexp_n_col( data.frame() ), 0 )
expect_equal( geometries:::.test_sexp_n_col( list(x=1) ), 1 )
expect_equal( geometries:::.test_sexp_n_col( matrix(1:4, ncol = 2) ), 2 )

expect_equal( geometries:::.test_sexp_n_row( 1 ), 1 )
expect_equal( geometries:::.test_sexp_n_row( 1:4 ), 1 )
expect_equal( geometries:::.test_sexp_n_row( data.frame() ), 0 )
expect_equal( geometries:::.test_sexp_n_row( data.frame(x = 1:2) ), 2 )
expect_equal( geometries:::.test_sexp_n_row( list() ), 0 )
expect_equal( geometries:::.test_sexp_n_row( list( data.frame(x = 1:2) ) ), 1 )  ## n_row is not-recursive

expect_equal( geometries:::.test_sexp_length( 1:4 ), 4 )
expect_equal( geometries:::.test_sexp_length( 1L:5L ), 5 )
expect_equal( geometries:::.test_sexp_length( c(T,F,T) ), 3 )
expect_equal( geometries:::.test_sexp_length( list() ), 0 )
expect_equal( geometries:::.test_sexp_length( data.frame( x = 1) ), 1 )
expect_equal( geometries:::.test_sexp_length( c(complex(3), complex(4) ) ), 7 )


