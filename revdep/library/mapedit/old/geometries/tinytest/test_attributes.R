
x <- 1:5
attr <- list(myClass = "myClassAttr")
geometries:::.test_attributes( x, attr )
expect_true( attr( x, "myClass" ) == "myClassAttr" )


## Test that both 'has_been_closed' and any other attributes get attached
## when creating geometries

df <- data.frame(
  id1 = c(1,1,1,1,1,1,1,1,1)
  , id2 = c(1,1,1,1,2,2,2,2,2)
  , x = c(1:8, 5)
  , y = c(1:8, 5)
)

res <- geometries:::rcpp_make_geometries(
  x = df
  , id_cols = c(0L,1L)
  , geometry_cols = c(2L,3L)
  , attributes = list(myClass = "myClass")
  , close = TRUE
  , closed_attribute = FALSE
)

expect_true( attr( res[[1]], "myClass") == "myClass" )
expect_true( is.null( attr( res[[1]][[1]], "has_been_closed") ) )

res <- geometries:::rcpp_make_geometries(
  x = df
  , id_cols = c(0L,1L)
  , geometry_cols = c(2L,3L)
  , attributes = list(myClass = "myClass")
  , close = TRUE
  , closed_attribute = TRUE
)

expect_true( attr( res[[1]], "myClass") == "myClass" )
expect_true( attr( res[[1]][[1]], "closed") == "has_been_closed" )
expect_true( is.null( attr( res[[1]][[2]], "closed") ) )


## has-been_closed not added if close == FALSE
res <- geometries:::rcpp_make_geometries(
  x = df
  , id_cols = c(0L,1L)
  , geometry_cols = c(2L,3L)
  , attributes = list(myClass = "myClass")
  , close = FALSE
  , closed_attribute = TRUE
)

expect_true( attr( res[[1]], "myClass") == "myClass" )
expect_true( is.null( attr( res[[1]][[1]], "closed") ) )
