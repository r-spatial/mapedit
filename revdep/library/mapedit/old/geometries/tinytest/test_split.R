

df <- data.frame(
  multi_id = c(1,1,1,1, 1,1,1,1,1, 1,1,1,1)
  , poly_id = c(1,1,1,1, 1,1,1,1,1, 2,2,2,2)
  , line_id = c(1,1,1,1, 2,2,2,2,2, 1,1,1,1)
  , x = c(0,0,1,1, 2,2,3,3,2, 10,10,12,12)
  , y = c(0,1,1,0, 2,3,3,2,2, 10,12,12,10)
  , z = c(1,2,2,2, 2,3,3,3,2, 3,2,2,3)
  #, prop = letters[1:13]
)

res <- geometries:::.test_split_by_id(
  df
  , ids = c(0L,1L,2L)
  , geometry_cols = c(3L,4L,5L)
  , last = TRUE
  , attributes = NULL
  , close = TRUE
  , closed_attribute = TRUE
)

expect_true( length( res$coords ) == 3 )
expect_true( attr(res$coords[[1]], "closed") == "has_been_closed" )
expect_true( is.null( attr(res$coords[[2]], "closed") ) )
expect_true( attr(res$coords[[3]], "closed") == "has_been_closed" )

