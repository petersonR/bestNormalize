library(testthat)
library(bestNormalize)

data(iris)
train <- iris$Petal.Width
boxcox_obj <- boxcox(train)

test_that('boxcox Transforms original data consistently', {
  expect_equal(boxcox_obj$x.t, predict(boxcox_obj))
  expect_equal(boxcox_obj$x, predict(boxcox_obj, inverse = T))
})

test_that('boxcox Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(boxcox_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(boxcox_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

