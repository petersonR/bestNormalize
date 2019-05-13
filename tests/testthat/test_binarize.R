context('binarize functionality')

data(iris)

train <- iris$Petal.Width
binarize.obj <- binarize(train)
bn2 <- binarize(train, "mean")
bn3 <- binarize(train, "mode")

test_that('binarize ransforms original data consistently' , {
  expect_equal(binarize.obj$x.t, predict(binarize.obj))
  expect_equal(bn2$x.t, predict(bn2))
  expect_equal(bn3$x.t, predict(bn3))
})

test_that('binarize Transforms new data', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(binarize.obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  pred <- predict(bn2, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  pred <- predict(bn3, newdata = nd)
  expect_true(!any(is.na(pred)))
})

test_that('binarize correctly handles missing original data', {
  b <- binarize(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), as.numeric(predict(b, inverse = TRUE)[1]))
  
  b <- binarize(c(NA, train), "mean")
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), as.numeric(predict(b, inverse = TRUE)[1]))
  
  b <- binarize(c(NA, train), "mode")
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), as.numeric(predict(b, inverse = TRUE)[1]))
})

test_that('binarize correctly handles missing new data', {
  b <- binarize(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), as.numeric(predict(b, newdata = c(1, NA), inverse = TRUE)[2]))
  
  b <- binarize(train, "mean")
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), as.numeric(predict(b, newdata = c(1, NA), inverse = TRUE)[2]))
  
  b <- binarize(train, "mode")
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), as.numeric(predict(b, newdata = c(1, NA), inverse = TRUE)[2]))
})