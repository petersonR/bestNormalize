#' Box-Cox Normalization
#' 
#' @name boxcox
#' @aliases predict.boxcox
#'   
#' @description Perform a Box-Cox transformation and center/scale a vector to
#'   attempt normalization
#' @param x A vector to normalize with Box-Cox
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param ... Additional arguments that can be passed to the estimation of the
#'   lambda parameter (lower, upper, epsilon)
#' @param object an object of class 'boxcox'
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @details \code{boxcox} estimates the optimal value of lambda for the Box-Cox
#' transformation. This transformation can be performed on new data, and
#' inverted, via the \code{predict} function.
#' 
#' The function will return an error if a user attempt to transform nonpositive
#' data.
#' 
#' 
#' @return A list of class \code{boxcox} with elements 
#' \item{x.t}{transformed 
#'   original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
#'   \item{lambda}{estimated lambda value for skew transformation} 
#'   \item{n}{number of nonmissing observations}
#'   \item{norm_stat}{Pearson's P / degrees of freedom}
#'   \item{standardize}{was the transformation standardized}
#'   
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'   
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#'   transformations. Journal of the Royal Statistical Society B, 26, 211-252.
#'   
#' @examples 
#' x <- rgamma(100, 1, 1)
#' 
#' bc_obj <- boxcox(x)
#' bc_obj
#' p <- predict(bc_obj)
#' x2 <- predict(bc_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' @seealso  \code{\link[MASS]{boxcox}}
#' @importFrom stats sd
#' @export
boxcox <- function(x, standardize = TRUE, ...) {
  stopifnot(is.numeric(x))
  l <- estimate_boxcox_lambda(x, ...)
  x.t <- boxcox_trans(x, l)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    lambda = l,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  class(val) <- c('boxcox', class(val))
  val
}

#' @rdname boxcox
#' @method predict boxcox
#' @export
predict.boxcox <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) newdata <- newdata * object$sd + object$mean
    newdata <-  inv_boxcox_trans(newdata, object$lambda)
  } else if (!inverse) {
    newdata <- boxcox_trans(newdata, object$lambda)
    if (object$standardize) newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname boxcox
#' @method print boxcox
#' @export
print.boxcox <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'Box Cox Transformation with', x$n, 'nonmissing obs.:\n', 
      'Estimated statistics:\n',
      '- lambda =', x$lambda, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}

# Modified version of boxcox from MASS package
#' @importFrom stats lm optimize
estimate_boxcox_lambda <- function(x, lower = -1, upper = 2, eps = .001) {
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  
  if (any(x <= 0))
    stop("x must be positive")
  
  log_x <- log(x)
  xbar <- exp(mean(log_x))
  
  fit <- lm(x ~ 1, data = data.frame(x = x))
  xqr <- fit$qr
  
  boxcox_loglik <- function(lambda) {
    if (abs(lambda) > eps)
      xt <- (x ^ lambda - 1) / lambda
    else 
      xt <- log_x * (1 + (lambda * log_x) / 2 * 
                       (1 + (lambda * log_x) / 3 * 
                          (1 + (lambda * log_x) / 4)))
    - n / 2 * log(sum(qr.resid(xqr, xt / xbar ^ (lambda - 1)) ^ 2))
  }
  
  results <- optimize(boxcox_loglik, lower = lower, 
                      upper = upper, maximum = TRUE,
                      tol = .0001)
  
  results$maximum
}

# Internal transformation functions
boxcox_trans <- function(x, lambda, eps = .001) {
  if (lambda < 0)
    x[x < 0] <- NA
  if (abs(lambda) < eps)
    val <- log(x)
  else
    val <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  val
}

inv_boxcox_trans <- function(x, lambda, eps = .001) {
  if (lambda < 0)
    x[x > -1 / lambda] <- NA
  if (abs(lambda) < eps)
    val <- exp(x)
  else {
    x <- x * lambda + 1
    val <- sign(x) * abs(x) ^ (1 / lambda)
  }
  val
}
