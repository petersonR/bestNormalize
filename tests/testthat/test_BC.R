library(testthat)
library(bestNormalize)

data(iris)
train <- iris$Petal.Width
BC.obj <- BC(train)

test_that('BC Transforms original data consistently', {
  expect_equal(BC.obj$x.t, predict.bc(BC.obj))
  expect_equal(BC.obj$x, predict.bc(BC.obj, inverse = T))
})

test_that('BC Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict.bc(BC.obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict.bc(BC.obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

