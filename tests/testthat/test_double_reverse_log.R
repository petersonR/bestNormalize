context('double_reverse_log functionality')

data(iris)
train <- iris$Petal.Width
double_reverse_log_obj <- double_reverse_log(train)

test_that('double_reverse_log Transforms original data consistently', {
  expect_equal(double_reverse_log_obj$x.t, predict(double_reverse_log_obj))
  expect_equal(double_reverse_log_obj$x, predict(double_reverse_log_obj, inverse = TRUE))
})

test_that('double_reverse_log Transforms new data consistently', {
  nd <- seq(0, 2.7, length = 100)
  pred <- predict(double_reverse_log_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(double_reverse_log_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('double_reverse_log correctly handles missing original data', {
  b <- double_reverse_log(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('double_reverse_log correctly handles missing new data', {
  b <- double_reverse_log(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
double_reverse_log_obj <- double_reverse_log(train, standardize = FALSE)

test_that('double_reverse_log Transforms original data consistently', {
  expect_equal(double_reverse_log_obj$x.t, predict(double_reverse_log_obj))
  expect_equal(double_reverse_log_obj$x, predict(double_reverse_log_obj, inverse = TRUE))
})

test_that('double_reverse_log Transforms new data consistently', {
  nd <- seq(1, 2.7, length = 100)
  pred <- predict(double_reverse_log_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(double_reverse_log_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('double_reverse_log correctly handles missing original data', {
  b <- double_reverse_log(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('double_reverse_log correctly handles missing new data', {
  b <- double_reverse_log(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})


double_reverse_log_obj <- double_reverse_log(train, b = exp(1))

test_that('double_reverse_log Transforms new data the same regardless of predict order', {
  nd <- seq(0, 2.7, length = 100)
  
  pred <- predict(double_reverse_log_obj, newdata = nd)
  nd2 <- predict(double_reverse_log_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
  
  p1 <- sapply(nd, function(nd_i) predict(double_reverse_log_obj, newdata = nd_i))
  p2 <- predict(double_reverse_log_obj, newdata = nd)
  expect_equal(p1, p2)
  
})

double_reverse_log_obj <- double_reverse_log(train, eps = 1.6)

test_that('double_reverse_log Transforms new data well if padding increased', {
  nd <- seq(0, 4, length = 100)
  
  pred <- predict(double_reverse_log_obj, newdata = nd)
  nd2 <- predict(double_reverse_log_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
  
  p1 <- sapply(nd, function(nd_i) predict(double_reverse_log_obj, newdata = nd_i))
  p2 <- predict(double_reverse_log_obj, newdata = nd)
  expect_equal(p1, p2)
  
})
