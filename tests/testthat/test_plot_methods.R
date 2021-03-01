context('plot method functionality')

data(iris)

train <- c(NA, -1, iris$Petal.Width)

bn <- suppressWarnings(bestNormalize(train, quiet = TRUE))
test_that('bestNormalize plot method runs (in-sample) without error or warning', {
  expect_silent(plot(bn))
})

m2plot <- names(bn$other_transforms)
m2plot <- m2plot[!(m2plot %in% c("exp_x"))]

test_that('bestNormalize plot method runs (out-of-sample) without error or warning', {
  expect_warning(plot(bn, bounds = c(-2, 10), methods = m2plot))
})

on <- suppressWarnings(orderNorm(train))
test_that('orderNorm plot method runs without error or warning', {
  expect_silent(plot(on))
  expect_silent(plot(on, bounds = c(-2, 10)))
})

test_that('lambert plot method runs without error or warning', {
  skip_on_cran()
  skip_on_travis()
  expect_silent(lw <- suppressWarnings(lambert(train)))
  expect_silent(plot(lw))
})

bc <- suppressWarnings(boxcox(train[train > 0]))
test_that('boxcox plot method runs without error or warning', {
  expect_silent(plot(bc))
})

yj <- suppressWarnings(yeojohnson(train))
test_that('yeojohnson plot method runs without error or warning', {
  expect_silent(plot(yj))
})
