context('print method functionality')

data(iris)

train <- c(NA, -1, iris$Petal.Width)

bn <- suppressWarnings(bestNormalize(train, quiet = T))
test_that('bestNormalize print method runs without error or warning', {
  expect_output(print(bn))
})

on <- suppressWarnings(orderNorm(train))
test_that('orderNorm print method runs without error or warning', {
  expect_output(print(on))
})

test_that('lambert print method runs without error or warning', {
  skip_on_cran()
  skip_on_travis()
  lw <- suppressWarnings(lambert(train))
  
  expect_output(print(lw))
})

bc <- suppressWarnings(boxcox(train[train > 0]))
test_that('boxcox print method runs without error or warning', {
  expect_output(print(bc))
})

ex <- suppressWarnings(exp_x(train))
test_that('exp_x print method runs without error or warning', {
  expect_output(print(ex))
})

lx <- suppressWarnings(log_x(train))
test_that('log_x print method runs without error or warning', {
  expect_output(print(lx))
})

nt <- suppressWarnings(no_transform(train))
test_that('no_transform print method runs without error or warning', {
  expect_output(print(nt))
})

sx <- suppressWarnings(sqrt_x(train))
test_that('sqrt_x print method runs without error or warning', {
  expect_output(print(sx))
})

yj <- suppressWarnings(yeojohnson(train))
test_that('yeojohnson print method runs without error or warning', {
  expect_output(print(yj))
})

bn <- suppressWarnings(binarize(train))
test_that('binarize print method runs without error or warning', {
  expect_output(print(bn))
})
