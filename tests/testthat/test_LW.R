library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width

LW.obj <- Lambert(train)

test_that('LW Transforms original data consistently' , {
  expect_equal(LW.obj$x.t, predict.lw(LW.obj))
  expect_equal(LW.obj$x, predict.lw(LW.obj, inverse = T))
})

test_that('LW Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict.lw(LW.obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict.lw(LW.obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})
