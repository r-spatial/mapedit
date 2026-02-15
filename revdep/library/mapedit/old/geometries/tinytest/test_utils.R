
df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
other_cols <- geometries:::rcpp_other_columns( df, NULL, NULL, NULL )
expect_equal( other_cols, c(0,1,2) )

m <- matrix( 1:24, ncol = 3 )
other_cols <- geometries:::rcpp_other_columns( m, NULL, NULL, NULL )
expect_equal( other_cols, c(0,1,2))


df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, NULL )
expect_equal( other_cols, c(0,1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(2L)
other_cols <- geometries:::rcpp_other_columns( m, id, NULL, NULL )
expect_equal( other_cols, c(0,1))


df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L,2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, NULL )
expect_equal( other_cols, c(1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(0L,2L)
other_cols <- geometries:::rcpp_other_columns( m, id, NULL, NULL )
expect_equal( other_cols, c(1))

## using names / strings
df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, NULL )
expect_equal( other_cols, c("x","y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( m, id, NULL, NULL )
expect_equal( other_cols, c("x","y"))

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, NULL )
expect_equal( other_cols, c("y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( m, id, NULL, NULL )
expect_equal( other_cols, c("y"))




df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, NULL )
expect_equal( other_cols, c(0,1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(2L)
other_cols <- geometries:::rcpp_other_columns( m, NULL, id, NULL )
expect_equal( other_cols, c(0,1))


df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L,2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, NULL )
expect_equal( other_cols, c(1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(0L,2L)
other_cols <- geometries:::rcpp_other_columns( m, NULL, id, NULL )
expect_equal( other_cols, c(1))

## using names / strings
df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, NULL )
expect_equal( other_cols, c("x","y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( m, NULL, id, NULL )
expect_equal( other_cols, c("x","y"))

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, NULL )
expect_equal( other_cols, c("y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( m, NULL, id, NULL )
expect_equal( other_cols, c("y"))



df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, NULL, NULL, id )
expect_equal( other_cols, c(0,1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(2L)
other_cols <- geometries:::rcpp_other_columns( m, NULL, NULL, id )
expect_equal( other_cols, c(0,1))


df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L,2L)  ## c++ index
other_cols <- geometries:::rcpp_other_columns( df, NULL, NULL, id )
expect_equal( other_cols, c(1) )

m <- matrix( 1:24, ncol = 3 )
id <- c(0L,2L)
other_cols <- geometries:::rcpp_other_columns( m, NULL, NULL, id )
expect_equal( other_cols, c(1))

## using names / strings
df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( df, NULL, NULL, id )
expect_equal( other_cols, c("x","y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z")
other_cols <- geometries:::rcpp_other_columns( m, NULL, NULL, id )
expect_equal( other_cols, c("x","y"))

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( df, NULL, NULL, id )
expect_equal( other_cols, c("y") )

m <- matrix( 1:24, ncol = 3 )
dimnames(m) <- list(NULL, c("x","y","z") )
id <- c("z","x")
other_cols <- geometries:::rcpp_other_columns( m, NULL, NULL, id )
expect_equal( other_cols, c("y"))



df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y")
other_cols <- geometries:::rcpp_other_columns( df, id, id2, NULL )
expect_equal( other_cols, c("x") )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y","x")
other_cols <- geometries:::rcpp_other_columns( df, id, id2, NULL )
expect_equal( other_cols, character() )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L)
id2 <- c(1L,2L)
other_cols <- geometries:::rcpp_other_columns( df, id, id2, NULL )
expect_equal( other_cols, numeric() )



df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y")
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, id2 )
expect_equal( other_cols, c("x") )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y","x")
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, id2 )
expect_equal( other_cols, character() )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L)
id2 <- c(1L,2L)
other_cols <- geometries:::rcpp_other_columns( df, NULL, id, id2 )
expect_equal( other_cols, numeric() )




df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y")
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, id2 )
expect_equal( other_cols, c("x") )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c("z")
id2 <- c("y","x")
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, id2 )
expect_equal( other_cols, character() )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6 )
id <- c(0L)
id2 <- c(1L,2L)
other_cols <- geometries:::rcpp_other_columns( df, id, NULL, id2 )
expect_equal( other_cols, numeric() )




df <- data.frame(x = 1:2, y = 3:4, z = 5:6, m = 7:8 )
id <- c(0L)
id2 <- c(1L,2L)
id3 <- c(3L)
other_cols <- geometries:::rcpp_other_columns( df, id, id2, id3 )
expect_equal( other_cols, numeric() )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6, m = 7:8, id = 1:2 )
id <- c(0L)
id2 <- c(1L,2L)
id3 <- c(3L)
other_cols <- geometries:::rcpp_other_columns( df, id, id2, id3 )
expect_equal( other_cols, c(4) )

df <- data.frame(x = 1:2, y = 3:4, z = 5:6, m = 7:8, id = 1:2 )
id <- c(0L)
id2 <- c(0L)
id3 <- c(0L)
other_cols <- geometries:::rcpp_other_columns( df, id, id2, id3 )
expect_equal( other_cols, c(1,2,3,4) )


### -----
### vectors

expect_equal( geometries:::rcpp_concatenate_vectors(1L,5L), c(1L,5L))
expect_equal( geometries:::rcpp_concatenate_vectors(1,5), c(1,5))
expect_equal( geometries:::rcpp_concatenate_vectors(1.2,5), c(1.2,5))
expect_equal( geometries:::rcpp_concatenate_vectors("a","b"), c("a","b"))
expect_error( geometries:::rcpp_concatenate_vectors(1,"a"), "geometries - different vector types found")

expect_equal( geometries:::rcpp_concatenate_vectors(c(1L,2L), c(3L)), c(1L:3L))
expect_equal( geometries:::rcpp_concatenate_vectors(c(1L,2L), c(3L,4L)), c(1L:4L))
expect_equal( geometries:::rcpp_concatenate_vectors(c("a","b"), c("c","d")), c("a","b","c","d"))

### ------
### columns

df <- data.frame(
  a = 1L:3L
  , b = 4L:6
)
m <- as.matrix( df )
expect_equal( geometries:::rcpp_column_positions( m, c("a") ), c(0) )
expect_equal( geometries:::rcpp_column_positions( m, c("a","b") ), c(0,1) )

df <- data.frame(
  a = 1.3
  , b = 4.6
)
m <- as.matrix( df )
expect_equal( geometries:::rcpp_column_positions( m, c("a") ), c(0) )
expect_equal( geometries:::rcpp_column_positions( m, c("a","b") ), c(0,1) )

sv <- c("hello", "world")
expect_equal( geometries:::rcpp_where_is( "hello" , sv ), 0L )
expect_equal( geometries:::rcpp_where_is( "world" , sv ), 1L )
expect_equal( geometries:::rcpp_where_is( "foo" , sv ), -1L )


m <- matrix(1L:4L, ncol = 2)
expect_equal( geometries:::rcpp_get_ids( m, 0L ), c(1L:2L) )

m <- matrix(c(1.2, 2,3,4), ncol = 2)
expect_equal( geometries:::rcpp_get_ids( m, 0L ), c(1.2,2) )

m <- matrix(1L:4L, ncol = 2)
df <- as.data.frame(m)
m <- as.matrix(m)
expect_equal( geometries:::rcpp_get_ids( m, 0L ), c(1L:2L) )
expect_equal( geometries:::rcpp_get_ids( m, "V1" ), c(1L:2L) )

m <- matrix(c(1.2, 2,3,4), ncol = 2)
df <- as.data.frame(m)
m <- as.matrix(m)
expect_equal( geometries:::rcpp_get_ids( m, 0L ), c(1.2,2) )
expect_equal( geometries:::rcpp_get_ids( m, "V1" ), c(1.2,2) )

