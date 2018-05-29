context('bestNormalize functionality')

data(iris)
train <- iris$Petal.Width

BNobject <- suppressWarnings(bestNormalize(train, out_of_sample = FALSE))
BNobject4 <- bestNormalize(train, allow_orderNorm = FALSE, out_of_sample = FALSE)

# Test transformations
test_that('BestNormalize transformations with positive data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

# 
BNobject <- suppressWarnings(bestNormalize(c(-1, train)))
BNobject4 <- suppressWarnings(bestNormalize(c(-1, train), allow_orderNorm = FALSE, out_of_sample = FALSE))
test_that('BestNormalize transformations with mixed data, out-of-sample', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})


BNobject <- suppressWarnings(bestNormalize(c(-train), out_of_sample = FALSE))
BNobject4 <- suppressWarnings(bestNormalize(c(-train), allow_orderNorm = FALSE, out_of_sample = FALSE))
test_that('BestNormalize transformations with negative data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, out_of_sample = FALSE))
BNobject4 <- suppressWarnings(bestNormalize(train2, allow_orderNorm = FALSE, out_of_sample = FALSE))
test_that('BestNormalize transformations with mixed data and missing values', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), out_of_sample = FALSE))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, out_of_sample = FALSE))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardize = FALSE
train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, standardize = FALSE, out_of_sample = FALSE))
BNobject4 <- suppressWarnings(bestNormalize(train2, standardize = FALSE, allow_orderNorm = FALSE, out_of_sample = FALSE))
test_that('BestNormalize transformations without standardization', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize without standardization handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), standardize = FALSE, out_of_sample = FALSE))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize without standardization handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, standardize = FALSE, out_of_sample = FALSE))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

