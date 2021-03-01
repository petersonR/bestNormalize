context('orderNorm functionality')

data(iris)

train <- iris$Petal.Width
orderNorm_obj <- suppressWarnings(orderNorm(train))

test_that('orderNorm transforms original data consistently', {
  expect_equal(orderNorm_obj$x.t, predict(orderNorm_obj))
  expect_equal(orderNorm_obj$x, predict(orderNorm_obj, inverse = TRUE))
})

test_that('orderNorm Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  expect_warning(pred <- predict(orderNorm_obj, newdata = nd))
  expect_true(!any(is.na(pred)))
  expect_warning(nd2 <- predict(orderNorm_obj, newdata = pred, inverse = TRUE))
  expect_equal(nd, nd2)
})

test_that('orderNorm correctly handles missing original data', {
  suppressWarnings(b <- orderNorm(c(NA, train)))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('orderNorm correctly handles missing new data', {
  suppressWarnings(b <- orderNorm(train))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})
