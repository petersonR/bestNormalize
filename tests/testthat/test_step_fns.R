context('step_* recipes functionality')
library(recipes)

# par(mfrow = c(2,1))
rec <- recipe(~ ., data = as.data.frame(iris))

test_that('step_* transformations with iris data', {
  dt1 <- orderNorm(iris$Petal.Length, warn = F)$x.t
  
  ## Using BoxCox

  expect_silent(bc_trans <- step_BoxCox(rec, all_numeric()))
  expect_silent(bc_estimates <- prep(bc_trans, training = as.data.frame(iris)))
  expect_silent(bc_data <- bake(bc_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bc_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bc_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bc_estimates, number = 1)), 4)
  
  ## Using bestNormalize
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric()))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "orderNorm")
  expect_identical(dt1, bn_data$Petal.Length)
  
  ## LOO 
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric(), transform_options = list(loo = TRUE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "log_x")

  ## Faster (use in-sample metrics, does NOT use orderNorm)
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric(), transform_options = list(out_of_sample = FALSE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris)))
  # plot(density(iris[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "log_x")

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
  dt1 <- orderNorm(iris2$Petal.Length, warn = F)$x.t
  
  ## Using BoxCox
  expect_silent(bc_trans <- step_BoxCox(rec, all_numeric()))
  expect_silent(bc_estimates <- prep(bc_trans, training = as.data.frame(iris2)))
  expect_silent(bc_data <- bake(bc_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bc_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bc_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bc_estimates, number = 1)), 3)
  
  ## Using bestNormalize
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric()))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "orderNorm")
  expect_identical(dt1, bn_data$Petal.Length)
  
  ## LOO 
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric(), transform_options = list(loo = TRUE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "yeojohnson")

  ## Faster (use in-sample metrics, does NOT use orderNorm)
  expect_silent(bn_trans <- step_bestNormalize(rec, all_numeric(), transform_options = list(out_of_sample = FALSE, allow_orderNorm = FALSE)))
  expect_silent(bn_estimates <- prep(bn_trans, training = as.data.frame(iris2)))
  expect_silent(bn_data <- bake(bn_estimates, as.data.frame(iris2)))
  # plot(density(iris2[, "Petal.Length"]), main = "before")
  # plot(density(bn_data$Petal.Length), main = "after")
  expect_equal(nrow(tidy(bn_trans, number = 1)), 1)
  expect_equal(nrow(tidy(bn_estimates, number = 1)), 4)
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]], "bestNormalize")
  expect_s3_class(tidy(bn_estimates, number = 1)$value[[3]]$chosen_transform, "sqrt_x")

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

