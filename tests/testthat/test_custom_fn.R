context('creating new fn for bestNormalize functionality')


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

# Optional: print method
print.cuberoot_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'cuberoot(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


data(iris)
train <- iris$Petal.Width
cuberoot_x_obj <- cuberoot_x(train)

test_that('cuberoot_x Transforms original data consistently', {
  expect_equal(cuberoot_x_obj$x.t, predict(cuberoot_x_obj))
  expect_equal(cuberoot_x_obj$x, predict(cuberoot_x_obj, inverse = T))
})

test_that('cuberoot_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(cuberoot_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(cuberoot_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('cuberoot_x correctly handles missing original data', {
  b <- cuberoot_x(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('cuberoot_x correctly handles missing new data', {
  b <- cuberoot_x(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardization
cuberoot_x_obj <- cuberoot_x(train, standardize = FALSE)

test_that('cuberoot_x Transforms original data consistently', {
  expect_equal(cuberoot_x_obj$x.t, predict(cuberoot_x_obj))
  expect_equal(cuberoot_x_obj$x, predict(cuberoot_x_obj, inverse = T))
})

test_that('cuberoot_x Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(cuberoot_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(cuberoot_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('cuberoot_x correctly handles missing original data', {
  b <- cuberoot_x(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('cuberoot_x correctly handles missing new data', {
  b <- cuberoot_x(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})


cuberoot_x_obj <- cuberoot_x(train, a = 1)

test_that('cuberoot_x Transforms new data consistently (given a)', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(cuberoot_x_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(cuberoot_x_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

## Run bestNormalize tests with new_transforms


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

# Optional: print method
print.quadroot_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'quadroot(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}

new_transforms <- list(
  quadroot_x = quadroot_x,
  predict.quadroot_x = predict.quadroot_x,
  print.quadroot_x = print.quadroot_x,
  cuberoot_x = cuberoot_x,
  predict.cuberoot_x = predict.cuberoot_x,
  print.cuberoot_x = print.cuberoot_x
)

train <- iris$Petal.Width

BNobject <- (bestNormalize(train, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
BNobject4 <- bestNormalize(train, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms)
BNobject5 <- (bestNormalize(train, out_of_sample = TRUE, quiet = T, new_transforms = new_transforms))

# Test transformations
test_that('BestNormalize transformations with positive data (with custom fns)', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

# 
BNobject <- suppressWarnings(bestNormalize(c(-1, train), quiet = T, new_transforms = new_transforms))
BNobject4 <- suppressWarnings(bestNormalize(c(-1, train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
test_that('BestNormalize transformations with mixed data, in-sample (with custom fns)', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})


BNobject <- suppressWarnings(bestNormalize(c(-train), out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
BNobject4 <- suppressWarnings(bestNormalize(c(-train), allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
test_that('BestNormalize transformations with negative data (with custom fns)', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
BNobject4 <- suppressWarnings(bestNormalize(train2, allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
test_that('bestNormalize transformations with mixed data and missing values (with custom fns)', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize handles missing original data  (with custom fns)', {
  suppressWarnings(b <- bestNormalize(c(NA, train), out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize handles missing new data  (with custom fns)', {
  suppressWarnings(b <- bestNormalize(train, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# Test standardize = FALSE
train2 <- c(train, -1, NA)
BNobject <- suppressWarnings(bestNormalize(train2, standardize = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
BNobject4 <- suppressWarnings(bestNormalize(train2, standardize = FALSE, 
                                            allow_orderNorm = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
test_that('BestNormalize transformations without standardization  (with custom fns)', {
  expect_equal(BNobject$x.t, predict(BNobject))
  expect_equal(BNobject$x, predict(BNobject, inverse = T))
  expect_equal(BNobject4$x.t, predict(BNobject4))
  expect_equal(BNobject4$x, predict(BNobject4, inverse = T))
})

test_that('bestNormalize without standardization handles missing original data  (with custom fns)', {
  suppressWarnings(b <- bestNormalize(c(NA, train), standardize = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('bestNormalize without standardization handles missing new data  (with custom fns)', {
  suppressWarnings(b <- bestNormalize(train, standardize = FALSE, out_of_sample = FALSE, quiet = T, new_transforms = new_transforms))
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

## Test lambert functionality in bestNormalize
test_that("bestNormalize works with lambert of type s (with custom fns)", {
  skip_on_travis()
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_s = TRUE, quiet = T, new_transforms = new_transforms))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h (with custom fns)", {
  skip_on_travis()
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T, new_transforms = new_transforms))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("bestNormalize works with lambert of type h (with custom fns)", {
  skip_on_travis()
  skip_on_cran()
  b <-  suppressWarnings(bestNormalize(train, allow_lambert_h = TRUE, quiet = T, new_transforms = new_transforms))
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
})

test_that("options work for bestNormalize and with new transforms", {
  ## 
  expect_silent(b <- bestNormalize(train, tr_opts = list(cuberoot_x = list(a = 1)), warn = F, new_transforms = new_transforms))
  expect_equal(b$other_transforms$cuberoot_x$a, 1)
  expect_silent(b <- bestNormalize(train, tr_opts = list(cuberoot_x = list(a = 100)), warn = F, new_transforms = new_transforms))
  expect_equal(b$other_transforms$cuberoot_x$a, 100)
  
})


# Error checking for custom functions
test_that("errors work for new transforms when improperly specified", {
  expect_error(b <- bestNormalize(train, warn = F, new_transforms = "not_a_transform"))
  
  # No predict method
  expect_error(b <- bestNormalize(train, warn = F, new_transforms = list(I)))
})



