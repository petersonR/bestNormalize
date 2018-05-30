context('log_x functionality')

data(iris)
train <- iris$Petal.Width
log_x_obj <- log_x(train)

test_that('log_x Transforms original data consistently', {
  expect_equal(log_x_obj$x.t, predict(log_x_obj))
  expect_equal(log_x_obj$x, predict(log_x_obj, inverse = T))
})

test_that('log_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(log_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(log_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('log_x correctly handles missing original data', {
  b <- log_x(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('log_x correctly handles missing new data', {
  b <- log_x(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
log_x_obj <- log_x(train, standardize = FALSE)

test_that('log_x Transforms original data consistently', {
  expect_equal(log_x_obj$x.t, predict(log_x_obj))
  expect_equal(log_x_obj$x, predict(log_x_obj, inverse = T))
})

test_that('log_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(log_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(log_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('log_x correctly handles missing original data', {
  b <- log_x(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('log_x correctly handles missing new data', {
  b <- log_x(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})


log_x_obj <- log_x(train, a = 1)

test_that('log_x Transforms new data consistently (given a)', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(log_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(log_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

log_x_obj <- log_x(train, a = 1, b = exp(1))

test_that('log_x Transforms new data consistently (given a and b)', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(log_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(log_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})
