context('no_transform functionality')

data(iris)
train <- iris$Petal.Width
no_transform_obj <- no_transform(train)

test_that('no_transform Transforms original data consistently', {
  expect_equal(no_transform_obj$x.t, predict(no_transform_obj))
  expect_equal(no_transform_obj$x, predict(no_transform_obj, inverse = TRUE))
})

test_that('no_transform Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(no_transform_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(no_transform_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('no_transform correctly handles missing original data', {
  b <- no_transform(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('no_transform correctly handles missing new data', {
  b <- no_transform(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

data(iris)
train <- iris$Petal.Width
center_scale_obj <- center_scale(train)

test_that('center_scale Transforms original data consistently', {
  expect_equal(center_scale_obj$x.t, predict(center_scale_obj))
  expect_equal(center_scale_obj$x, predict(center_scale_obj, inverse = TRUE))
})

test_that('center_scale Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(center_scale_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(center_scale_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('center_scale correctly handles missing original data', {
  b <- center_scale(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('center_scale correctly handles missing new data', {
  b <- center_scale(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

