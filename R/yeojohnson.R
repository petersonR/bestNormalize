#' Yeo-Johnson Normalization
#' 
#' @name yeojohnson
#' @aliases predict.yeojohnson
#'   
#' @description Perform a Yeo-Johnson Transformation and center/scale a vector to
#'   attempt normalization
#' @param x A vector to normalize with Yeo-Johnson
#' @param eps A value to compare lambda against to see if it is equal to zero
#' @param ... Additional arguments that can be passed to the estimation of the
#'   lambda parameter (lower, upper)
#' @details \code{yeojohnson} estimates the optimal value of lamda for the Yeo-Johnson
#' transformation. This transformation can be performed on new data, and
#' inverted, via the \code{predict} function.
#' 
#' The Yeo-Johnson is similar to the Box-Cox method, however it allows for the
#' transformation of nonpositive data as well.
#' 
#' @return A list of class \code{yeojohnson} with elements 
#' \item{x.t}{transformed original data} 
#' \item{x}{original data} 
#' \item{mean}{mean of vector post-YJ transformation} 
#' \item{sd}{sd of vector post-BC transformation} 
#' \item{lambda}{estimated lambda value for skew transformation} 
#' \item{n}{number of nonmissing observations}
#' \item{norm_stat}{Pearson's chi-squared normality test statistic}
#'   
#' The \code{predict} function returns the numeric value of the transformation
#' performed on new data, and allows for the inverse transformation as well.
#'   
#'@references Yeo, I. K., & Johnson, R. A. (2000). A new family of power 
#'  transformations to improve normality or symmetry. Biometrika.
#'  
#'  Max Kuhn and Hadley Wickham (2017). recipes: Preprocessing Tools to Create 
#'  Design Matrices. R package version 0.1.0.9000. 
#'  https://github.com/topepo/recipes
#' 
#' @examples 
#' 
#' x <- rgamma(100, 1, 1)
#' 
#' yeojohnson_obj <- yeojohnson(x)
#' yeojohnson_obj
#' p <- predict(yeojohnson_obj)
#' x2 <- predict(yeojohnson_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @seealso  \code{\link[recipes]{recipes::step_YeoJohnson}}
#' @export
yeojohnson <- function(x, eps = .001, ...) {
  stopifnot(is.numeric(x))
  lambda <- estimate_yeojohnson_lambda(x, eps, ...)
  x.t <- x
  na_idx <- is.na(x)
  x.t[!na_idx] <- yeojohnson_trans(x[!na_idx], lambda, eps)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  x.t <- (x.t - mu) / sigma
  
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    lambda = lambda,
    n = length(x.t) - sum(na_idx),
    norm_stat = unname(nortest::pearson.test(x.t)$stat)
  )
  class(val) <- 'yeojohnson'
  val
}

#' @rdname yeojohnson
#' @method predict yeojohnson
#' @export
predict.yeojohnson <- function(yeojohnson_obj,
                               newdata = NULL,
                               inverse = FALSE) {
  if (is.null(newdata) & !inverse)
    newdata <- yeojohnson_obj$x
  if (is.null(newdata))
    newdata <- yeojohnson_obj$x.t
  
  na_idx <- is.na(newdata)
  
  if (inverse) {
    newdata <- newdata * yeojohnson_obj$sd + yeojohnson_obj$mean
    newdata[!na_idx] <- inv_yeojohnson_trans(newdata[!na_idx], yeojohnson_obj$lambda)
  } else {
    newdata[!na_idx] <- yeojohnson_trans(newdata[!na_idx], yeojohnson_obj$lambda)
    newdata <- (newdata - yeojohnson_obj$mean) / yeojohnson_obj$sd
  }
  
  unname(newdata)
}

#' @rdname yeojohnson
#' @method print yeojohnson
#' @export
print.yeojohnson <- function(yeojohnson_obj) {
  cat('Yeo-Johnson Transformation with', yeojohnson_obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      '- lambda =', yeojohnson_obj$lambda, '\n',
      '- mean =', yeojohnson_obj$mean, '\n',
      '- sd =', yeojohnson_obj$sd, '\n')
}

# Helper functions that estimates yj lambda parameter
estimate_yeojohnson_lambda <- function(x, lower = -5, upper = 5, eps = .001) {
  n <- length(x)
  ccID <- complete.cases(x)
  x <- x[ccID]
  

  # See references, Yeo & Johnson Biometrika (2000)
  yj_loglik <- function(lambda) {
    x_t <- yeojohnson_trans(x, lambda, eps)
    x_t_bar <- mean(x_t)
    x_t_var <- var(x_t) * (n - 1) / n
    constant <- sum(sign(x) * log(abs(x) + 1))
    -0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }
  
  results <- optimize(yj_loglik, lower = lower, 
                      upper = upper, maximum = TRUE,
                      tol = .0001)
  
  results$maximum
}

yeojohnson_trans <- function(x, lambda, eps = .001) {
  pos_idx <- x >= 0
  neg_idx <- x < 0
  
  # Transform negative values
  if (any(pos_idx)) {
    if (abs(lambda) < eps) {
      x[pos_idx] <- log(x[pos_idx] + 1)
    } else {
      x[pos_idx] <- ((x[pos_idx] + 1) ^ lambda - 1) / lambda
    }
  } 
  
  # Transform nonnegative values
  if (any(neg_idx)){
    if (abs(lambda - 2) < eps) {
      x[neg_idx] <- - log(-x[neg_idx] + 1)
    } else {
      x[neg_idx] <- - ((-x[neg_idx] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
    }
  }
  x
}

inv_yeojohnson_trans <- function(x, lambda, eps = .001) {
  val <- x
  neg_idx <- x < 0
  
  if(any(!neg_idx)) {
    if(abs(lambda) < eps) {
      val[!neg_idx] <- exp(x[!neg_idx]) - 1
    } else {
      val[!neg_idx] <- (x[!neg_idx] * lambda + 1) ^ (1 / lambda) - 1
    }
  }
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      val[neg_idx] <- -expm1(-x[neg_idx])
    } else {
      val[neg_idx] <- 1 - (-(2 - lambda) * x[neg_idx] + 1) ^ (1 / (2 - lambda))
    }
  }
  val
}
