
l <- list(1:5, 1:5)
df <- data.frame( x = 1:5, y = 1:5 )
expect_equal( geometries:::.test_make_dataframe( l, 5, c("x","y")), df )

# l <- list()
# expect_equal( geometries:::.test_make_dataframe( l, 0, "x"), data.frame(x = numeric() ) )
