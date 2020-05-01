context('no_transform functionality')

data(iris)
train <- iris$Petal.Width
no_transform_obj <- no_transform(train)

test_that('no_transform Transforms original data consistently', {
  expect_equal(no_transform_obj$x.t, predict(no_transform_obj))
  expect_equal(no_transform_obj$x, predict(no_transform_obj, inverse = T))
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

