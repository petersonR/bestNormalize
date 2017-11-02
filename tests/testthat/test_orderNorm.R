library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width
orderNorm.obj <- suppressWarnings(orderNorm(train))

test_that('orderNorm transforms original data consistently', {
  expect_equal(orderNorm.obj$x.t, predict.orderNorm(orderNorm.obj))
  expect_equal(orderNorm.obj$x, predict.orderNorm(orderNorm.obj, inverse = T))
})

test_that('orderNorm Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  expect_warning(pred <- predict(orderNorm.obj, newdata = nd))
  expect_true(!any(is.na(pred)))
  expect_warning(nd2 <- predict(orderNorm.obj, newdata = pred, inverse = TRUE))
  expect_equal(nd, nd2)
})
