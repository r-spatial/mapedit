
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c(1)), 0 )
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c(2)), 1 )
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c(1,2)), c(0,1) )
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c("x")), 0 )
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c("y")), 1 )
expect_equal( geometries:::index_correct( data.frame(x = 1, y = 2), cols = c("x","y")), c(0,1) )

expect_equal( geometries:::index_correct( matrix(1:4, ncol = 2 ), cols = c(1,2) ), c(0,1) )
df <- data.frame(x = 1:2, y = 3:4); m <- as.matrix( df )
expect_equal( geometries:::index_correct( m , cols = c("x","y") ), c(0,1) )

df <- data.frame(
  id1 = c(1,1,1,1,2,2,2)
  , id2 = c(1,1,2,2,1,1,2)
  , id3 = c(1,2,3,4,1,2,3)
  , x = 1:7
  , y = 1:7
  , z = 1:7
  , m = 1:7
  , a = 1:7
)

expect_error(
  geometries:::rcpp_make_geometries( df, c(0L), c("y"), TRUE, TRUE, FALSE )
  , "geometries - id_columns and geometry_columns must be the same type"
  )

expect_error(
  geometries:::rcpp_make_geometries( df, c(1L), c("y"), TRUE, TRUE, FALSE )
  , "geometries - id_columns and geometry_columns must be the same type"
)

m <- as.matrix( df )

## indexing crash
expect_error(
  gm_geometries(df, c(0L),  c("x","y"), list( class = "my_attr") )
  , "invalid column index"
)


## NULL column ids
expect_error( gm_geometries(df, id_cols = NULL, c("x","y"), list( class = "my_attr") ), "columns can't be NULL")


## single ID, single geometry, no attributes
res <- gm_geometries(df, 1, 4)
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 1 )
expect_true( ncol( res[[2]] ) == 1 )

res <- gm_geometries(m, 1, 4)
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 1 )
expect_true( ncol( res[[2]] ) == 1 )

## single ID, single geometry, attributes
res <- gm_geometries(df, 1, 4, list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 1 )
expect_true( ncol( res[[2]] ) == 1 )
expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

res <- gm_geometries(m, 1, 4, list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 1 )
expect_true( ncol( res[[2]] ) == 1 )
expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

## single ID, multi geometry, no attributes
res <- gm_geometries(df, 1, c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 5 )
expect_true( ncol( res[[2]] ) == 5 )

res <- gm_geometries(m, 1, c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 5 )
expect_true( ncol( res[[2]] ) == 5 )


## single ID, multi geometry, no attributes
res <- gm_geometries(df, 1, c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 5 )
expect_true( ncol( res[[2]] ) == 5 )
expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

res <- gm_geometries(m, 1, c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( ncol( res[[1]] ) == 5 )
expect_true( ncol( res[[2]] ) == 5 )
expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

## multi ID, multi geometry, no attributes
res <- gm_geometries(df, c(1,2), c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( length( res[[1]] ) == 2 )
expect_true( length( res[[2]] ) == 2 )

expect_true( ncol( res[[1]][[1]] ) == 5 )
expect_true( ncol( res[[1]][[2]] ) == 5 )

expect_true( ncol( res[[2]][[1]] ) == 5 )
expect_true( ncol( res[[2]][[2]] ) == 5 )

expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

## attributes not set on inner objects
expect_true( is.null( attr( res[[1]][[1]], "class")  ) )
expect_true( is.null( attr( res[[1]][[2]], "class")  ) )
expect_true( is.null( attr( res[[2]][[1]], "class")  ) )
expect_true( is.null( attr( res[[1]][[2]], "class")  ) )



res <- gm_geometries(m, c(1,2), c(4,5,6,7,8), list( class = "my_attr") )
expect_true( length( res ) == 2 )
expect_true( length( res[[1]] ) == 2 )
expect_true( length( res[[2]] ) == 2 )

expect_true( ncol( res[[1]][[1]] ) == 5 )
expect_true( ncol( res[[1]][[2]] ) == 5 )

expect_true( ncol( res[[2]][[1]] ) == 5 )
expect_true( ncol( res[[2]][[2]] ) == 5 )

expect_true( attr( res[[1]], "class") == "my_attr" )
expect_true( attr( res[[2]], "class") == "my_attr" )

## attributes not set on inner objects
expect_true( is.null( attr( res[[1]][[1]], "class")  ) )
expect_true( is.null( attr( res[[1]][[2]], "class")  ) )
expect_true( is.null( attr( res[[2]][[1]], "class")  ) )
expect_true( is.null( attr( res[[1]][[2]], "class")  ) )

## shapes are closed

