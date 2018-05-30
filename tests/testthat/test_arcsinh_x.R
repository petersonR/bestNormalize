context('arcsinh_x functionality')

data(iris)
train <- iris$Petal.Width
arcsinh_x_obj <- arcsinh_x(train)

test_that('arcsinh_x Transforms original data consistently', {
  expect_equal(arcsinh_x_obj$x.t, predict(arcsinh_x_obj))
  expect_equal(arcsinh_x_obj$x, predict(arcsinh_x_obj, inverse = T))
})

test_that('arcsinh_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(arcsinh_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(arcsinh_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('arcsinh_x correctly handles missing original data', {
  b <- arcsinh_x(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('arcsinh_x correctly handles missing new data', {
  b <- arcsinh_x(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
arcsinh_x_obj <- arcsinh_x(train, standardize = FALSE)

test_that('arcsinh_x Transforms original data consistently', {
  expect_equal(arcsinh_x_obj$x.t, predict(arcsinh_x_obj))
  expect_equal(arcsinh_x_obj$x, predict(arcsinh_x_obj, inverse = T))
})

test_that('arcsinh_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(arcsinh_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(arcsinh_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('arcsinh_x correctly handles missing original data', {
  b <- arcsinh_x(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('arcsinh_x correctly handles missing new data', {
  b <- arcsinh_x(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

