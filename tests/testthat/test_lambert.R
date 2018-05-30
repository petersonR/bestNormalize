context('lambert functionality')

## LambertW package doesn't work quite right on linux and 
# solaris machines (it uses too many threads)
skip_on_os("linux")
skip_on_os("solaris")

data(iris)

train <- iris$Petal.Width

lambert_obj <- lambert(train)

test_that('lambert Transforms original data consistently' , {
  expect_equal(lambert_obj$x.t, predict(lambert_obj))
  expect_equal(lambert_obj$x, predict(lambert_obj, inverse = T))
})

test_that('lambert Transforms new data consistently', {
  nd <- seq(-1, 4, length = 100)
  pred <- predict(lambert_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(lambert_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

lambert_obj <- lambert(train, standardize = FALSE)

test_that('lambert without standardization Transforms original data consistently' , {
  expect_equal(lambert_obj$x.t, predict(lambert_obj))
  expect_equal(lambert_obj$x, predict(lambert_obj, inverse = T))
})

test_that('lambert without standardization Transforms new data consistently', {
  nd <- seq(-1, 4, length = 100)
  pred <- predict(lambert_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(lambert_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})


# For type = 'hh'

lambert_obj <- lambert(train, type = 'hh')

test_that('lambert Transforms original data consistently' , {
  expect_equal(lambert_obj$x.t, predict(lambert_obj))
  expect_equal(lambert_obj$x, 
               predict(lambert_obj, inverse = T), 
               tolerance = .001)
})

test_that('lambert Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(lambert_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(lambert_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2, tolerance = .001)
})


# for type = 'h'

lambert_obj <- lambert(train, type = 'h')

test_that('lambert Transforms original data consistently' , {
  expect_equal(lambert_obj$x.t, predict(lambert_obj))
  expect_equal(lambert_obj$x, predict(lambert_obj, inverse = T))
})

test_that('lambert Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(lambert_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(lambert_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2, tolerance = .001)
})

test_that('lambert correctly handles missing original data', {
  b <- lambert(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('lambert correctly handles missing new data', {
  b <- lambert(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})
