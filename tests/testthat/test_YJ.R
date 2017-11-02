library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width

YJ.obj <- YJ(train)

test_that('YJ Transforms original data consistently', {
  expect_equal(YJ.obj$x.t, predict.yj(YJ.obj))
  expect_equal(YJ.obj$x, predict.yj(YJ.obj, inverse = T))
})

test_that('YJ Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(YJ.obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(YJ.obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})
