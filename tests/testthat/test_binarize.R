library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width
binarize.obj <- binarize(train)

test_that('binarize ransforms original data consistently' , {
  expect_equal(binarize.obj$x.t, predict.binarize(binarize.obj))
})

test_that('LW Transforms new data', {
  nd <- seq(0, 4, length = 100)
  pred <- predict.binarize(binarize.obj, newdata = nd)
  expect_true(!any(is.na(pred)))
})
