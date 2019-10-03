#'Yeo-Johnson Normalization
#'
#'@name yeojohnson
#'@aliases predict.yeojohnson
#'
#'@description Perform a Yeo-Johnson Transformation and center/scale a vector to
#'  attempt normalization
#'@param x A vector to normalize with Yeo-Johnson
#'@param eps A value to compare lambda against to see if it is equal to zero
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#'@param ... Additional arguments that can be passed to the estimation of the
#'  lambda parameter (lower, upper)
#'@param newdata a vector of data to be (reverse) transformed
#'@param inverse if TRUE, performs reverse transformation
#'@param object an object of class 'yeojohnson'
#'@details \code{yeojohnson} estimates the optimal value of lambda for the
#'  Yeo-Johnson transformation. This transformation can be performed on new
#'  data, and inverted, via the \code{predict} function.
#'
#'  The Yeo-Johnson is similar to the Box-Cox method, however it allows for the
#'  transformation of nonpositive data as well. The \code{step_YeoJohnson}
#'  function in the \code{recipes} package is another useful resource (see
#'  references).
#'
#'@return A list of class \code{yeojohnson} with elements
#'
#'   \item{x.t}{transformed original data} 
#'   \item{x}{original data}
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
#'   \item{lambda}{estimated lambda value for skew transformation}
#'   \item{n}{number of nonmissing observations} 
#'   \item{norm_stat}{Pearson's P / degrees of freedom} 
#'   \item{standardize}{Was the transformation standardized}
#'
#'  The \code{predict} function returns the numeric value of the transformation
#'  performed on new data, and allows for the inverse transformation as well.
#'
#'@references Yeo, I. K., & Johnson, R. A. (2000). A new family of power
#'  transformations to improve normality or symmetry. Biometrika.
#'
#'  Max Kuhn and Hadley Wickham (2017). recipes: Preprocessing Tools to Create
#'  Design Matrices. R package version 0.1.0.9000.
#'  https://github.com/topepo/recipes
#'
#'
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
#'@importFrom stats sd
#'@export
yeojohnson <- function(x, eps = .001, standardize = TRUE, ...) {
  stopifnot(is.numeric(x))
  lambda <- estimate_yeojohnson_lambda(x, eps = eps, ...)
  x.t <- x
  na_idx <- is.na(x)
  x.t[!na_idx] <- yeojohnson_trans(x[!na_idx], lambda, eps)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    lambda = lambda,
    eps = eps,
    n = length(x.t) - sum(na_idx),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  class(val) <- 'yeojohnson'
  val
}

#' @rdname yeojohnson
#' @method predict yeojohnson
#' @export
predict.yeojohnson <- function(object,
                               newdata = NULL,
                               inverse = FALSE, 
                               ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata))
    newdata <- object$x.t
  
  na_idx <- is.na(newdata)
  
  if (inverse) {
    if(object$standardize) newdata <- newdata * object$sd + object$mean
    newdata[!na_idx] <- inv_yeojohnson_trans(newdata[!na_idx], object$lambda)
  } else {
    newdata[!na_idx] <- yeojohnson_trans(newdata[!na_idx], object$lambda)
    if(object$standardize) newdata <- (newdata - object$mean) / object$sd
  }
  
  unname(newdata)
}

#' @rdname yeojohnson
#' @method print yeojohnson
#' @export
print.yeojohnson <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'Yeo-Johnson Transformation with', x$n, 'nonmissing obs.:\n', 
      'Estimated statistics:\n',
      '- lambda =', x$lambda, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}

# Helper functions that estimates yj lambda parameter
#' @importFrom stats var optimize
estimate_yeojohnson_lambda <- function(x, lower = -5, upper = 5, eps = .001) {
  n <- length(x)
  ccID <- !is.na(x)
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
