context('bestNormalize functionality')

data(iris)
train <- iris$Petal.Width

BNobject <- suppressWarnings(bestNormalize(train, out_of_sample = FALSE, quiet = T))
BNobject4 <- bestNormalize(train, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T)
BNobject5 <- suppressWarnings(bestNormalize(train, out_of_sample = TRUE, quiet = T))

# Test transformations
test_that('BestNormalize transformations with positive data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

# 
BNobject <- suppressWarnings(bestNormalize(c(-1, train), quiet = T))
BNobject4 <- suppressWarnings(bestNormalize(c(-1, train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T))
test_that('BestNormalize transformations with mixed data, in-sample', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})


BNobject <- suppressWarnings(bestNormalize(c(-train), out_of_sample = FALSE, quiet = T))
BNobject4 <- suppressWarnings(bestNormalize(c(-train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T))
test_that('BestNormalize transformations with negative data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, out_of_sample = FALSE, quiet = T))
BNobject4 <- suppressWarnings(bestNormalize(train2, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T))
test_that('BestNormalize transformations with mixed data and missing values', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), out_of_sample = FALSE, quiet = T))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, out_of_sample = FALSE, quiet = T))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardize = FALSE
train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, standardize = FALSE, out_of_sample = FALSE, quiet = T))
BNobject4 <- suppressWarnings(bestNormalize(train2, standardize = FALSE, 
                                            allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T))
test_that('BestNormalize transformations without standardization', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize without standardization handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), standardize = FALSE, out_of_sample = FALSE, quiet = T))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize without standardization handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, standardize = FALSE, out_of_sample = FALSE, quiet = T))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

## Test lambert functionality in bestNormalize
test_that("bestNormalize works with lambert of type s", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_s = TRUE, quiet = T))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("options work for bestNormalize", {
  ## Log_x
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 1)), warn = F))
  expect_equal(b$other_transforms$log_x$a, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 100)), warn = F))
  expect_equal(b$other_transforms$log_x$a, 100)
  
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 1, b = 5)), warn = F))
  expect_equal(b$other_transforms$log_x$b, 5)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 100, b = 10)), warn = F))
  expect_equal(b$other_transforms$log_x$b, 10)
  
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(eps = 1)), warn = F))
  expect_equal(b$other_transforms$log_x$eps, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(eps = 100)), warn = F))
  expect_equal(b$other_transforms$log_x$eps, 100)
  
  ## Sqrt_x
  expect_silent(b <- bestNormalize(train, tr_opts = list(sqrt_x = list(a = 1)), warn = F))
  expect_equal(b$other_transforms$sqrt_x$a, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(sqrt_x = list(a = 100)), warn = F))
  expect_equal(b$other_transforms$sqrt_x$a, 100)
  
  ## yeo_johnson
  expect_silent(b <- bestNormalize(train, tr_opts = list(yeojohnson = list(eps = 0.1)), warn = F))
  expect_equal(b$other_transforms$yeojohnson$eps, .1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(yeojohnson = list(eps = .01)), warn = F))
  expect_equal(b$other_transforms$yeojohnson$eps, .01)
})
