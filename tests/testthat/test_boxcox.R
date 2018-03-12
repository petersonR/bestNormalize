context('boxcox functionality')

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

test_that('boxcox does not try to estimate with negatives' , {
  expect_error(boxcox(c(-1, 2, 3)))
})

test_that('boxcox correctly handles missing original data', {
  b <- boxcox(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('boxcox correctly handles missing new data', {
  b <- boxcox(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
boxcox_obj <- boxcox(train, standardize = FALSE)

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

test_that('boxcox does not try to estimate with negatives' , {
  expect_error(boxcox(c(-1, 2, 3), standardize = FALSE))
})

test_that('boxcox correctly handles missing original data', {
  b <- boxcox(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('boxcox correctly handles missing new data', {
  b <- boxcox(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})
