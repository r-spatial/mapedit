library(classInt)
# issue #1
set.seed(101)
data_censored <- c(rep(0,10), rnorm(100, mean=20,sd=1),rep(26,10))
expect_error(classInt::classIntervals(data_censored, style = "quantile", 
  probs = seq(0, 1, 0.25)))

