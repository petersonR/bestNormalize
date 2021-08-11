context('bestNormalize parallel functionality')

skip_on_cran()
skip_on_ci()

data(iris)
train <- iris$Petal.Width

cl <- parallel::makeCluster(2, setup_strategy = "sequential")

test_that("Parallel functionality works for RCV", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, r = 2, quiet = TRUE)
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_s = TRUE, r = 2, quiet = TRUE)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, allow_lambert_s = FALSE, r = 2, quiet = TRUE)
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, allow_lambert_s = TRUE, r = 2, quiet = TRUE)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

train <- iris$Petal.Width

test_that("Parallel functionality works for LOO", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, loo = TRUE, quiet = TRUE)
  b <- bestNormalize(train, warn = FALSE, allow_lambert_h = TRUE, 
                     allow_lambert_s = TRUE, loo = TRUE, quiet = FALSE)
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
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, r = 2, quiet = TRUE, new_transforms = new_transforms))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_s = TRUE, 
                                   r = 2, quiet = TRUE, new_transforms = new_transforms))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                                   r = 2, quiet = TRUE, new_transforms = new_transforms))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
  expect_silent(b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                                   allow_lambert_s = TRUE, r = 2, quiet = TRUE, new_transforms = new_transforms))
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

train <- iris$Petal.Width

test_that("Parallel functionality works for LOO", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, loo = TRUE, quiet = TRUE, new_transforms = new_transforms)
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                     allow_lambert_s = TRUE, loo = TRUE, quiet = TRUE, new_transforms = new_transforms)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 


parallel::stopCluster(cl)

