library(testthat)
library(bestNormalize)

test_check(package = 'bestNormalize', filter = "arcsinh|binarize|exp_x|log|no_transform|sqrt")
