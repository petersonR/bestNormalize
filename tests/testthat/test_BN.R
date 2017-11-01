library(testthat)
library(bestNormalize)

data(iris)
train <- iris$Petal.Width

test_that('Various options work for BN with positive data', {
  expect_silent(BNobject <- bestNormalize(train))
  expect_warning(BNobject2 <- bestNormalize(train, D_max = .16))
  expect_warning(BNobject3 <- bestNormalize(train, D_max = .16, allow_orderNorm = FALSE))
  expect_silent(BNobject4 <- bestNormalize(train, D_max = 1, allow_orderNorm = FALSE))
})

test_that('Various options work for BN with mixed data', {
  expect_silent(BNobject <- bestNormalize(c(train, -1)))
  expect_warning(BNobject2 <- bestNormalize(c(train, -1), D_max = .16))
  expect_warning(BNobject3 <- bestNormalize(c(train, -1), D_max = .16, allow_orderNorm = FALSE))
  expect_silent(BNobject4 <- bestNormalize(c(train, -1), D_max = 1, allow_orderNorm = FALSE))
})

test_that('Various options work for BN with negative data', {
  expect_silent(BNobject <- bestNormalize(-train))
  expect_warning(BNobject2 <- bestNormalize(-train, D_max = .16))
  expect_warning(BNobject3 <- bestNormalize(-train, D_max = .16, allow_orderNorm = FALSE))
  expect_silent(BNobject4 <- bestNormalize(-train, D_max = 1, allow_orderNorm = FALSE))
})


BNobject <- bestNormalize(train)
suppressWarnings(BNobject2 <- bestNormalize(train, D_max = .16))
BNobject4 <- bestNormalize(train, D_max = 1, allow_orderNorm = FALSE)

# Test transformations
test_that('BestNormalize transformations work with positive data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject2$x.t, predict.bestNormalize(BNobject2))
  expect_equal(BNobject2$x, predict.bestNormalize(BNobject2, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})

# 
BNobject <- bestNormalize(c(train, -1))
suppressWarnings(BNobject2 <- bestNormalize(c(train, -1), D_max = .16))
BNobject4 <- bestNormalize(c(train, -1), D_max = 1, allow_orderNorm = FALSE)
test_that('BestNormalize transformations work with mixed data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject2$x.t, predict.bestNormalize(BNobject2))
  expect_equal(BNobject2$x, predict.bestNormalize(BNobject2, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})


BNobject <- bestNormalize(-train)
suppressWarnings(BNobject2 <- bestNormalize(-train, D_max = .16))
BNobject4 <- bestNormalize(-train, D_max = 1, allow_orderNorm = FALSE)
test_that('BestNormalize transformations work with negative data', {
  expect_equal(BNobject$x.t, predict.bestNormalize(BNobject))
  expect_equal(BNobject$x, predict.bestNormalize(BNobject, inverse = T))
  expect_equal(BNobject2$x.t, predict.bestNormalize(BNobject2))
  expect_equal(BNobject2$x, predict.bestNormalize(BNobject2, inverse = T))
  expect_equal(BNobject4$x.t, predict.bestNormalize(BNobject4))
  expect_equal(BNobject4$x, predict.bestNormalize(BNobject4, inverse = T))
})