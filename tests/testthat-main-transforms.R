library(testthat)
library(bestNormalize)

test_check(package = 'bestNormalize', filter = "boxcox|yeojohnson|lambert|orderNorm")
