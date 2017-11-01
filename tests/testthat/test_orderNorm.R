library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width
orderNorm.obj <- suppressWarnings(orderNorm(train))

test_that('orderNorm transforms original data consistently', {
  expect_equal(orderNorm.obj$x.t, predict.orderNorm(orderNorm.obj))
  expect_equal(orderNorm.obj$x, predict.orderNorm(orderNorm.obj, inverse = T))
})

# Note that orderNorm will not actually transform new data consistently
# With the given smoother
