
m <- matrix(1:4, ncol = 2)
l1 <- list( m )
l2 <- list( l1 )
l3 <- list( l2 )
l4 <- list( l3 )
l5 <- list( l4 )

res <- geometries:::gm_dimensions( m )
expect_true( res$max_nest == 0 )

res <- geometries:::gm_dimensions( l1 )
expect_true( res$max_nest == 1 )

res <- geometries:::gm_dimensions( l2 )
expect_true( res$max_nest == 2 )

res <- geometries:::gm_dimensions( l3 )
expect_true( res$max_nest == 3 )

res <- geometries:::gm_dimensions( l4 )
expect_true( res$max_nest == 4 )

res <- geometries:::gm_dimensions( l5 )
expect_true( res$max_nest == 5 )


expect_equal( geometries:::rcpp_nest( l1, 1 ), l1 )
expect_equal( geometries:::rcpp_nest( l1, 2 ), l2 )
expect_equal( geometries:::rcpp_nest( l1, 3 ), l3 )
expect_equal( geometries:::rcpp_nest( l1, 4 ), l4 )
expect_equal( geometries:::rcpp_nest( l1, 5 ), l5 )

expect_equal( geometries:::rcpp_nest( l5, 1 ), l1 )
expect_equal( geometries:::rcpp_nest( l5, 2 ), l2 )
expect_equal( geometries:::rcpp_nest( l5, 3 ), l3 )
expect_equal( geometries:::rcpp_nest( l5, 4 ), l4 )
expect_equal( geometries:::rcpp_nest( l5, 5 ), l5 )


## representative of a POLYGON
l <- list(m, m)

## representative of converting to a MULTIPOLYGON
res <- geometries:::rcpp_nest( l, 2 )
expect_true( length( res ) == 1 )
expect_true( length( res[[1]] ) == 2 )

## No impact, beause it's max-nested at level 1 anyway
## geometries:::gm_dimensions( l )
res <- geometries:::rcpp_nest( l, 1 )
expect_equal( l, res )

# geometries:::gm_dimensions( l )$max_dimension

## No change
expect_equal(
  geometries:::rcpp_nest( l, 0 )
  , l
)

## what about mixed-depth lists
## the 'depth' is relative to the max depth

l <- list(
  1:3
  , list(
    list(3:1)
  )
)

# geometries:::gm_dimensions( l )
# max_dimension == 3
expect_equal(
  geometries:::rcpp_nest( l, 3 )
  , l
)

## relative to the max dimension,
## the first element won't get unnested any further than a single 'list'
## The first element starts at nesting level '1'
## so unnesting it doesn't have any effect
expect_equal(
  geometries:::rcpp_nest( l, 0 )
  , geometries:::rcpp_nest( l, 1 )
)

expect_equal(
  geometries:::rcpp_nest( l, 0 )[[1]]
  , geometries:::rcpp_nest( l, 2 )[[1]]
)


## strings work
l <- list(list(c("a", "b", "c", "d"), c("e", "f", "g", "h")))
expect_equal(
  geometries:::rcpp_nest( l, 1 )
  , l[[1]]
)

## representative of converting to LINESTRINGs
# geometries:::rcpp_nest( l, 1 )
# DOes this make sense?
# LINESTRING is a MATRIX
# so it's nesting is 0
# But if there are more than one, we can't unnest to 2 matrices, because that's not a SEXP
# so it has to be at least a list of matrices
# same with POINTs and MULTIPOINTs


# ## TODO
# ## list of shapes (like a sfc)
# ## Needs to iterate over the list and nest/unnest each shape individually
# l <- list(
#   l1,
#   l2
# )
#
# expect_equal( geometries:::rcpp_nest(l, 3), list( list( l ) ) )


