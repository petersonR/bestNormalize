context('bestNormalize with non-default norm_stat_fn')

data(iris)
train <- iris$Petal.Width

new_norm_stat_fn <- function(x) {
  nortest::lillie.test(x)$stat
}

BNobject <- suppressWarnings(bestNormalize(train, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
BNobject4 <- bestNormalize(train, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn)
BNobject5 <- suppressWarnings(bestNormalize(train, out_of_sample = TRUE, quiet = T, norm_stat_fn = new_norm_stat_fn))

# Test transformations
test_that('BestNormalize transformations with positive data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

# 
BNobject <- suppressWarnings(bestNormalize(c(-1, train), quiet = T, norm_stat_fn = new_norm_stat_fn))
BNobject4 <- suppressWarnings(bestNormalize(c(-1, train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
test_that('BestNormalize transformations with mixed data, in-sample', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})


BNobject <- suppressWarnings(bestNormalize(c(-train), out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
BNobject4 <- suppressWarnings(bestNormalize(c(-train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
test_that('BestNormalize transformations with negative data', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
BNobject4 <- suppressWarnings(bestNormalize(train2, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
test_that('BestNormalize transformations with mixed data and missing values', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardize = FALSE
train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, standardize = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
BNobject4 <- suppressWarnings(bestNormalize(train2, standardize = FALSE, 
                                            allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
test_that('BestNormalize transformations without standardization', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize without standardization handles missing original data', {
  suppressWarnings(b <- bestNormalize(c(NA, train), standardize = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize without standardization handles missing new data', {
  suppressWarnings(b <- bestNormalize(train, standardize = FALSE, out_of_sample = FALSE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

## Test lambert functionality in bestNormalize
test_that("bestNormalize works with lambert of type s", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_s = TRUE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h", {
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T, norm_stat_fn = new_norm_stat_fn))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("options work for bestNormalize", {
  ## Log_x
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 1)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$a, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 100)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$a, 100)
  
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 1, b = 5)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$b, 5)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(a = 100, b = 10)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$b, 10)
  
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(eps = 1)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$eps, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(log_x = list(eps = 100)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$log_x$eps, 100)
  
  ## Sqrt_x
  expect_silent(b <- bestNormalize(train, tr_opts = list(sqrt_x = list(a = 1)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$sqrt_x$a, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(sqrt_x = list(a = 100)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$sqrt_x$a, 100)
  
  ## yeo_johnson
  expect_silent(b <- bestNormalize(train, tr_opts = list(yeojohnson = list(eps = 0.1)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$yeojohnson$eps, .1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(yeojohnson = list(eps = .01)), warn = F, norm_stat_fn = new_norm_stat_fn))
  expect_equal(b$other_transforms$yeojohnson$eps, .01)
})

skip_on_cran()
skip_on_travis()

data(iris)
train <- iris$Petal.Width[rep(1:nrow(iris), each = 3)]

cl <- parallel::makeCluster(2, setup_strategy = "sequential")

test_that("Parallel functionality works for RCV", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, r = 2, quiet = T, norm_stat_fn = new_norm_stat_fn)
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_s = TRUE, r = 2, quiet = T, norm_stat_fn = new_norm_stat_fn)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, r = 2, quiet = T, norm_stat_fn = new_norm_stat_fn)
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, allow_lambert_s = TRUE, r = 2, quiet = T, norm_stat_fn = new_norm_stat_fn)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

train <- iris$Petal.Width

test_that("Parallel functionality works for LOO", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, loo = T, quiet = T, norm_stat_fn = new_norm_stat_fn)
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                     allow_lambert_s = TRUE, loo = T, quiet = T, norm_stat_fn = new_norm_stat_fn)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

## Test custom functions and parallelization

## Define user-function
cuberoot_x <- function(x, a = NULL, standardize = TRUE) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE)))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x))) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
  
  
  x.t <- (x + a)^(1/3)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  # Get in-sample normality statistic results
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    a = a,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  
  # Assign class
  class(val) <- c('cuberoot_x', class(val))
  val
}

predict.cuberoot_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  
  # If no data supplied and not inverse
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  
  # If no data supplied and inverse
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  # Actually performing transformations
  
  # Perform inverse transformation as estimated
  if (inverse) {
    
    # Reverse-standardize
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    
    # Reverse-cube-root (cube)
    newdata <-  newdata^3 - object$a
    
    
    # Otherwise, perform transformation as estimated
  } else if (!inverse) {
    # Take cube root
    newdata <- (newdata + object$a)^(1/3)
    
    # Standardize to mean 0, sd 1
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  
  # Return transformed data
  unname(newdata)
}

## Define user-function
quadroot_x <- function(x, a = NULL, standardize = TRUE) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE)))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x))) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
  
  
  x.t <- (x + a)^(1/4)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  # Get in-sample normality statistic results
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    a = a,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  
  # Assign class
  class(val) <- c('quadroot_x', class(val))
  val
}

predict.quadroot_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  
  # If no data supplied and not inverse
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  
  # If no data supplied and inverse
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  # Actually performing transformations
  
  # Perform inverse transformation as estimated
  if (inverse) {
    
    # Reverse-standardize
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    
    # Reverse-quad-root (quad)
    newdata <-  newdata^4 - object$a
    
    
    # Otherwise, perform transformation as estimated
  } else if (!inverse) {
    # Take quad root
    newdata <- (newdata + object$a)^(1/4)
    
    # Standardize to mean 0, sd 1
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  
  # Return transformed data
  unname(newdata)
}


new_transforms <- list(
  quadroot_x = quadroot_x,
  predict.quadroot_x = predict.quadroot_x,
  cuberoot_x = cuberoot_x,
  predict.cuberoot_x = predict.cuberoot_x
)

test_that("Parallel functionality works for RCV", {
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, r = 2, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_s = TRUE, 
                                   r = 2, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                                   r = 2, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                                   allow_lambert_s = TRUE, r = 2, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

train <- iris$Petal.Width

test_that("Parallel functionality works for LOO", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, loo = T, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn)
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                     allow_lambert_s = TRUE, loo = T, quiet = T, new_transforms = new_transforms, norm_stat_fn = new_norm_stat_fn)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 


parallel::stopCluster(cl)

