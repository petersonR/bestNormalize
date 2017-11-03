#' Box-Cox Normalization
#' 
#' @name boxcox
#' @aliases predict.boxcox
#'   
#' @description Perform a Box-Cox transformation and center/scale a vector to
#'   attempt normalization
#' @param x A vector to normalize with Box-Cox
#' @param ... Additional arguments that can be passed to the estimation of the
#'   lambda parameter (lower, upper, epsilon)
#' @details \code{boxcox} estimates the optimal value of lamda for the Box-Cox
#' transformation. This transformation can be performed on new data, and
#' inverted, via the \code{predict} function.
#' 
#' The function will return an error if a user attempt to transform nonpositive
#' data.
#' 
#' 
#' @return A list of class \code{boxcox} with elements \item{x.t}{transformed
#'   original data} \item{x}{original data} \item{mean}{mean of vector post-BC
#'   transformation} \item{sd}{sd of vector post-BC transformation} 
#'   \item{lambda}{estimated lambda value for skew transformation} \item{n}{size
#'   of vector}
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
#' @export
boxcox <- function(x, ...) {
  l <- estimate_boxcox_lambda(x, ...)
  x.t <- as.vector(forecast::BoxCox(x, l))
  mu <- mean(x.t)
  sigma <- sd(x.t)
  x.t <- (x.t - mu) / sigma
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    lambda = l,
    n = length(x.t)
  )
  class(val) <- c('boxcox', class(val))
  val
}

#' @rdname boxcox
#' @method predict boxcox
#' @export
predict.boxcox <- function(boxcox_obj,
                       newdata = NULL,
                       inverse = F) {
  if (is.null(newdata) & !inverse)
    newdata <- boxcox_obj$x
  if (is.null(newdata) & inverse)
    newdata <- boxcox_obj$x.t
  
  if (inverse) {
    newdata <- newdata * boxcox_obj$sd + boxcox_obj$mean
    newdata <-  forecast::InvBoxCox(newdata, boxcox_obj$lambda)
  } else if (!inverse) {
    newdata <- forecast::BoxCox(newdata, boxcox_obj$lambda)
    newdata <- (newdata - boxcox_obj$mean) / boxcox_obj$sd
  }
  unname(newdata)
}

#' @rdname boxcox
#' @method print boxcox
#' @export
print.boxcox <- function(boxcox_obj) {
  cat('Box Cox Transformation with', boxcox_obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      '- lamda =', boxcox_obj$lambda, '\n',
      '- mean =', boxcox_obj$mean, '\n',
      '- sd =', boxcox_obj$sd, '\n')
}

# Modified version of boxcox from MASS package
estimate_boxcox_lambda <- function(x, lower = -1, upper = 2, eps = .001) {
  n <- length(x)
  ccID <- complete.cases(x)
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


