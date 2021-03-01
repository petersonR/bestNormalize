context('yeojohnson functionality')

data(iris)

train <- iris$Petal.Width

yeojohnson_obj <- yeojohnson(train)

test_that('yeojohnson Transforms original data consistently', {
  expect_equal(yeojohnson_obj$x.t, predict(yeojohnson_obj))
  expect_equal(yeojohnson_obj$x, predict(yeojohnson_obj, inverse = TRUE))
})

test_that('yeojohnson Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(yeojohnson_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(yeojohnson_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('yeojohnson correctly handles missing original data', {
  b <- yeojohnson(c(NA, train))
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('yeojohnson correctly handles missing new data', {
  b <- yeojohnson(train)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})

# without standardization

yeojohnson_obj <- yeojohnson(train, standardize = FALSE)

test_that('yeojohnson without standardization Transforms original data consistently', {
  expect_equal(yeojohnson_obj$x.t, predict(yeojohnson_obj))
  expect_equal(yeojohnson_obj$x, predict(yeojohnson_obj, inverse = TRUE))
})

test_that('yeojohnson without standardization Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(yeojohnson_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(yeojohnson_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})

test_that('yeojohnson without standardization correctly handles missing original data', {
  b <- yeojohnson(c(NA, train), standardize = FALSE)
  expect_equal(as.numeric(NA), b$x.t[1])
  expect_equal(as.numeric(NA), predict(b)[1])
  expect_equal(as.numeric(NA), predict(b, inverse = TRUE)[1])
})

test_that('yeojohnson without standardization correctly handles missing new data', {
  b <- yeojohnson(train, standardize = FALSE)
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA))[2])
  expect_equal(as.numeric(NA), predict(b, newdata = c(1, NA), inverse = TRUE)[2])
})
