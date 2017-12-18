context('bestNormalize functionality')

data(iris)
train <- iris$Petal.Width

test_that('Various options for BN with positive data', {
  expect_warning(BNobject <- bestNormalize(train))
  expect_silent(BNobject3 <- bestNormalize(train, allow_orderNorm = FALSE, k = 5))
  expect_warning(BNobject <- bestNormalize(train, out_of_sample = FALSE))
})

test_that('Various options for BN with mixed data', {
  expect_warning(BNobject2 <- bestNormalize(c(train, -1)))
  expect_warning(BNobject3 <- bestNormalize(c(train, -1), allow_orderNorm = FALSE))
  expect_warning(BNobject4 <- bestNormalize(c(train, -1), allow_orderNorm = FALSE, out_of_sample = FALSE))
})

test_that('Various options for BN with negative data', {
  expect_warning(BNobject2 <- bestNormalize(-train))
  expect_warning(BNobject3 <- bestNormalize(-train, allow_orderNorm = FALSE))
  expect_warning(BNobject4 <- bestNormalize(-train, allow_orderNorm = FALSE, out_of_sample = FALSE))
})


BNobject <- suppressWarnings(bestNormalize(train))
BNobject4 <- bestNormalize(train, allow_orderNorm = FALSE, k = 5)

# Test transformations
test_that('BestNormalize transformations with positive data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})

# 
BNobject <- suppressWarnings(bestNormalize(c(-1, train)))
BNobject4 <- suppressWarnings(bestNormalize(c(-1, train), allow_orderNorm = FALSE))
test_that('BestNormalize transformations with mixed data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})


BNobject <- suppressWarnings(bestNormalize(c(-train)))
BNobject4 <- suppressWarnings(bestNormalize(c(-train), allow_orderNorm = FALSE))
test_that('BestNormalize transformations with negative data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})

train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2))
BNobject4 <- suppressWarnings(bestNormalize(train2, allow_orderNorm = FALSE))
test_that('BestNormalize transformations with mixed data and missing values', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})

test_that('bestNormalize handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train)))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize handles missing new data', {
  suppressWarnings(b <- bestNormalize(train))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})
