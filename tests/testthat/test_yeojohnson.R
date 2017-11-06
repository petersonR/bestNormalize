library(testthat)
library(bestNormalize)

data(iris)

train <- iris$Petal.Width

yeojohnson_obj <- yeojohnson(train)

test_that('yeojohnson Transforms original data consistently', {
  expect_equal(yeojohnson_obj$x.t, predict.yeojohnson(yeojohnson_obj))
  expect_equal(yeojohnson_obj$x, predict.yeojohnson(yeojohnson_obj, inverse = T))
})

test_that('yeojohnson Transforms new data consistently', {
  nd <- seq(0, 4, length = 100)
  pred <- predict(yeojohnson_obj, newdata = nd)
  expect_true(!any(is.na(pred)))
  
  nd2 <- predict(yeojohnson_obj, newdata = pred, inverse = TRUE)
  expect_equal(nd, nd2)
})
