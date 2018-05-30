context('exp_x functionality')

data(iris)
train <- iris$Petal.Width
exp_x_obj <- exp_x(train)

test_that('exp_x Transforms original data consistently', {
  expect_equal(exp_x_obj$x.t, predict(exp_x_obj))
  expect_equal(exp_x_obj$x, predict(exp_x_obj, inverse = T))
})

test_that('exp_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(exp_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(exp_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('exp_x correctly handles missing original data', {
  b <- exp_x(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('exp_x correctly handles missing new data', {
  b <- exp_x(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
exp_x_obj <- exp_x(train, standardize = FALSE)

test_that('exp_x Transforms original data consistently', {
  expect_equal(exp_x_obj$x.t, predict(exp_x_obj))
  expect_equal(exp_x_obj$x, predict(exp_x_obj, inverse = T))
})

test_that('exp_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(exp_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(exp_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('exp_x correctly handles missing original data', {
  b <- exp_x(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('exp_x correctly handles missing new data', {
  b <- exp_x(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

