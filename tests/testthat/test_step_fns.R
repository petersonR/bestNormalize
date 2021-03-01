context('step_* recipes functionality')
library(recipes)

# par(mfrow = c(2,1))
rec <- recipe(~ ., data = as.data.frame(iris))

test_that('step_* transformations with iris data', {
  dt1 <- orderNorm(iris$Petal.Length, warn = FALSE)$x.t
  
  ## Using BoxCox

  expect_silent(bc_trans <- step_BoxCox(rec, all_numeric()))
  expect_silent(bc_estimates <- prep(bc_trans, training = as.data.frame(iris)))
  expect_silent(bc_data <- bake(bc_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bc_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bc_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bc_estimates, number = 1)), 4)
  
  ## Using bestNormalize 
  
  # Check deprecation
  expect_warning(bn_trans <- step_bestNormalize(rec, all_numeric()))
  
  # Check step_best_normalize
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric()))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_equal(tidy(bn_estimates, number = 1)$value[[3]]$transform[1], "orderNorm")
  expect_identical(dt1, bn_data$Petal.Length)
  
  ## LOO 
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric(), transform_options = list(loo = TRUE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$tr_object[[1]], "log_x")

  ## Faster (use in-sample metrics, does NOT use orderNorm)
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric(), transform_options = list(out_of_sample = FALSE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$tr_object[[1]], "log_x")

  ## Fastest (only use ORQ (orderNorm) transformation)
  expect_silent(orq_trans <- step_orderNorm(rec, all_numeric()))
  expect_silent(orq_estimates <- prep(orq_trans, training = as.data.frame(iris)))
  expect_silent(orq_data <- bake(orq_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(orq_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(orq_trans, number = 1)), 1)
  expect_equal(nrow(tidy(orq_estimates, number = 1)), 4)
  expect_s3_class(tidy(orq_estimates, number = 1)$value[[3]], "orderNorm")
  expect_identical(dt1, orq_data$Petal.Length)
  
})

iris2 <- iris
iris2$Petal.Length[c(5,10, 19)] <- c(NA, -1, -3)
iris2$group <- rep(1:2, nrow(iris)/2)

test_that('step_* transformations with missing/negative/discrete data', {
  dt1 <- orderNorm(iris2$Petal.Length, warn = FALSE)$x.t
  
  ## Using BoxCox
  expect_silent(bc_trans <- step_BoxCox(rec, all_numeric()))
  expect_silent(bc_estimates <- prep(bc_trans, training = as.data.frame(iris2)))
  expect_silent(bc_data <- bake(bc_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bc_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bc_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bc_estimates, number = 1)), 3)
  
  ## Using bestNormalize
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric()))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$tr_object[[1]], "orderNorm")
  expect_identical(dt1, bn_data$Petal.Length)
  
  ## LOO 
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric(), transform_options = list(loo = TRUE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  # expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$tr_object[[1]], "lambert")

  ## Faster (use in-sample metrics, does NOT use orderNorm)
  expect_silent(bn_trans <- step_best_normalize(rec, all_numeric(), transform_options = list(out_of_sample = FALSE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$tr_object[[1]], "sqrt_x")

  ## Fastest (only use ORQ (orderNorm) transformation)
  expect_silent(orq_trans <- step_orderNorm(rec, all_numeric()))
  expect_silent(orq_estimates <- prep(orq_trans, training = as.data.frame(iris2)))
  expect_silent(orq_data <- bake(orq_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(orq_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(orq_trans, number = 1)), 1)
  expect_equal(nrow(tidy(orq_estimates, number = 1)), 4)
  expect_s3_class(tidy(orq_estimates, number = 1)$value[[3]], "orderNorm")
  expect_identical(dt1, orq_data$Petal.Length)
})

## A little help from butcher package
terms_empty_env <- function(axed, step_number) {
  expect_identical(attr(axed$steps[[step_number]]$terms[[1]], ".Environment"),
                   rlang::base_env())
}

impute_empty_env <- function(axed, step_number) {
  expect_identical(attr(axed$steps[[step_number]]$impute_with[[1]], ".Environment"),
                   rlang::base_env())
}

test_that("recipe + step_best_normalize + axe_env() works", {
  rec <- recipe(~ ., data = as.data.frame(state.x77)) %>%
    step_best_normalize(rec, all_numeric())
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})

test_that("recipe + step_orderNorm + axe_env() works", {
  rec <- recipe(~ ., data = as.data.frame(state.x77)) %>%
    step_orderNorm(rec, all_numeric())
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})

