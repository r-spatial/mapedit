
df <- data.frame(x = 1:5 )
expect_equal( geometries:::.test_unique_ids( df, NULL ), 1L )
expect_equal( geometries:::.test_unique_ids( df, "x" ), 1:5 )
expect_equal( geometries:::.test_unique_ids( df, 0L ), 1:5 )
expect_error( geometries:::.test_unique_ids( df, 1L ),  "geometries - column index out of range" )
expect_error( geometries:::.test_unique_ids( df, 1.3), "geometries - can't determine id column type" )
expect_error( geometries:::.test_unique_ids( df, -1L), "geometries - column index out of range")
expect_error( geometries:::.test_unique_ids( df, 3L), "geometries - column index out of range")

expect_error( geometries:::.test_unique_ids( list(), "x" ), "geometries - could not get id column")


expect_equal( geometries:::.test_unique_sort( c(1L,3L,3L,2L) ), c(1L,3L,2L) )
expect_equal( geometries:::.test_unique_sort( c(1,3,3,2) ), c(1,3,2) )
expect_equal( geometries:::.test_unique_sort( c(T,F,F,T) ), c(T,F) )

## Sorting factor - levels remain
f <- as.factor(c(1,2,3))
expect_equal(geometries:::.test_unique_sort(f), f)

df <- data.frame(fact = f)
expect_equal(geometries:::.test_unique_ids(df, 0L), f)
