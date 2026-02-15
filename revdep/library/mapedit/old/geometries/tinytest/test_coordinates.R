
## this represents a MULTIPOLYGON
l <- list(
  list(
    list(
      1:4
    )
    , list(
      3:5
    )
    , list(
      3:6
    )
    , list(
      1:4
    )
  )
)

## The max-nest should be 3, not 4
res <- geometries:::gm_dimensions(l)
expect_equal(res$max_nest, 3)
expect_equal(res$dimensions[, 4], res$max_nest)


## This geometry (sfg) would be invalid, because the coordinates are at different levels
## But it's still ok to calculate the dimensions, because it can represent an 'sfc'
l <- list(
  list(
    list(
      1:4
      , 4:1
      , list(1:5)
    )
  )
)

res <- geometries:::gm_dimensions( l )

expect_equal(res$max_nest, 4)
expect_equal(res$max_nest, res$dimensions[, 4])


## Another example
l <- list(
  list(
    list(
      1:4
    )
    , list(
      3:5
    )
    , list(
      3:6
    )
    , list(
      1:4
      , 4:1
    )
  )
)

## The max-nest should be 3, not 4
res <- geometries:::gm_dimensions(l)
expect_equal(res$max_nest, 3)
expect_equal(res$dimensions[, 4], res$max_nest)




x <- 1L:3L
res <- geometries:::gm_dimensions( x )

expect_true( ncol(res$dimensions) == 5)
expect_true( all(names(res) == c("dimensions", "max_dimension","max_nest")))

dims <- res$dimensions
expect_true( dims[1,1] == 0 ) ## always starts at index 0
expect_true( dims[1,2] == 0 ) ## vector only has 1 row
expect_true( dims[1,3] == 3 ) ## dimension (3 values in a coordinate)
expect_true( dims[1,4] == 0 ) ## nest - a vector is not nested in a list
expect_true( dims[1,5] == 13 ) ## integer

l <- list( x )
res <- geometries:::gm_dimensions( l )

dims <- res$dimensions
expect_true( dims[1,1] == 0 ) ## always starts at index 0
expect_true( dims[1,2] == 0 ) ## vector only has 1 row
expect_true( dims[1,3] == 3 ) ## dimension (3 values in a coordinate)
expect_true( dims[1,4] == 1 ) ## nest - inside a list
expect_true( dims[1,5] == 13 ) ## integer

m <- matrix(1:6, ncol = 2)
res <- geometries:::gm_dimensions( m )

dims <- res$dimensions
expect_true( dims[1,1] == 0 ) ## always starts at index 0
expect_true( dims[1,2] == 2 )
expect_true( dims[1,3] == 2 ) ## dimension (3 values in a coordinate)
expect_true( dims[1,4] == 0 ) ## nest - inside a list
expect_true( dims[1,5] == 13 ) ## integer

l <- list(
  1:3 ## point XYZ
  , matrix(1:6, ncol = 2) #line XY
  , list(
    matrix(1:6, ncol = 3) ## polygon XYZM
  )
  , matrix(1:8, ncol = 4) ## line XYZM
  # , list(
  #   list(
  #     matrix(1:6, ncol = 3) ## Multipolygon
  #   )
  # )
)

res <- geometries:::gm_dimensions( l )
dims <- res$dimensions
expect_true( all(dims[, 3] == c(3,2,3,4)))
expect_true( all(dims[, 4] == c(1,1,2,1)))


x <- 1:3
res <- gm_coordinates( x )
expect_true( ncol(res) == 4 )

m <- matrix(1:12, ncol = 3)
res <- gm_coordinates( m )
expect_true( ncol(res) == 4 )

l <- list(
  matrix(1:12, ncol = 2 )
)
res <- gm_coordinates( l )
expect_true( ncol( res ) == 4 )  ## nested in a list gets an id?

l <- list(
  matrix(1:12, ncol = 4 )
)
res <- gm_coordinates( l )
expect_true( ncol( res ) == 6 )

l <- list(
  list(
    matrix(1:12, ncol = 2)
  )
)
res <- gm_coordinates( l )
expect_true( ncol( res ) == 5 )

l <- list(
  list(
    matrix(1:12, ncol = 2)
    , matrix(1:4, ncol = 2)
  )
)
res <- gm_coordinates( l )
expect_true( ncol( res ) == 5 )
expect_true( unique( res$id ) == 1 )  ## first vector is id of the geometry
expect_equal( unique( res$id1 ), c(1,2) )  ## second vector is id of shape within the geometry

## nesting depth
l <- list(
  list(
    list(
      list(
        list(
          matrix(1:6, ncol = 2)
        )
      )
    )
  )
)
res <- gm_coordinates( l )
expect_equal( ncol( res ), 8 )

# ## and nesting works on dimenions
# res <- gm_dimensions( l )
# expect_equal( res$dimensions[, 3], 2 )
# expect_equal( res$dimensions[, 4], 5 )

l <- list(
  matrix(1:4, ncol = 2)
  , list(
    matrix(1:9, ncol = 3)
  )
)
res <- gm_coordinates( l )
expect_true( ncol( res ) == 6 )

expect_equal(
  l[[1]]
  , unname( as.matrix( res[1:2, c("c1","c2") ] ) )
)

expect_equal(
  l[[2]][[1]]
  , unname( as.matrix( res[3:5, c("c1","c2", "c3") ] ) )
)

## ID index starts at 1
expect_true( all( res[1:2, 1] == c(1,1) ) )

df <- data.frame(
  multi_id = c(1,1,1,1, 1,1,1,1,1, 1,1,1,1)
  , poly_id = c(1,1,1,1, 1,1,1,1,1, 2,2,2,2)
  , line_id = c(1,1,1,1, 2,2,2,2,2, 1,1,1,1)
  , x = c(0,0,1,1, 2,2,3,3,2, 10,10,12,12)
  , y = c(0,1,1,0, 2,3,3,2,2, 10,12,12,10)
  , z = c(1,2,2,2, 2,3,3,3,2, 3,2,2,3)
  #, prop = letters[1:13]
)

res <- geometries::gm_geometries(
  obj = df
  , id_cols = c(1L,2L,3L)
  , geometry_cols = c(4L,5L)
  , class_attributes = NULL
  , close = TRUE
  , closed_attribute = TRUE
)

## Now they will be closed
expect_true( length( res[[1]][[1]] ) == 2 )
expect_true( attr( res[[1]][[1]][[1]], "closed" ) == "has_been_closed" )
expect_true( is.null( attr( res[[1]][[1]][[2]], "closed") ) )
expect_true( attr( res[[1]][[2]][[1]], "closed" ) == "has_been_closed" )

## Now we can return how many closures there have been
dims <- geometries:::gm_dimensions( res )
expect_true( nrow( dims$dimensions ) == 1 ) ## one sfg

