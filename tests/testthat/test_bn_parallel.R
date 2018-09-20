context('bestNormalize parallel functionality')

skip_on_cran()
skip_on_travis()

data(iris)
train <- iris$Petal.Width[rep(1:nrow(iris), each = 3)]

cl <- parallel::makeCluster(2)

test_that("Parallel functionality works for RCV", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, r = 2, quiet = T)
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_s = TRUE, r = 2, quiet = T)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, r = 2, quiet = T)
  expect_true(is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, allow_lambert_s = TRUE, r = 2, quiet = T)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

train <- iris$Petal.Width

test_that("Parallel functionality works for LOO", {
  b <- bestNormalize(train, warn = FALSE, cluster = cl, loo = T, quiet = T)
  b <- bestNormalize(train, warn = FALSE, cluster = cl, allow_lambert_h = TRUE, 
                     allow_lambert_s = TRUE, loo = T, quiet = T)
  expect_true(!is.null(b$other_transforms$lambert_s))
  expect_true(!is.null(b$other_transforms$lambert_h))
  
}) 

parallel::stopCluster(cl)

