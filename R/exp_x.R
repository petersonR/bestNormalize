#' exp(x) Transformation
#' 
#' @name exp_x
#' @aliases predict.exp_x
#'   
#' @description Perform a exp(x) transformation 
#' @param x A vector to normalize with with x
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param warn Should a warning result from infinite values?
#' @param object an object of class 'exp_x'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{exp_x} performs a simple exponential transformation in the context of 
#' bestNormalize, such that it creates a transformation that can be estimated
#' and applied to new data via the \code{predict} function. 
#'  
#' @return A list of class \code{exp_x} with elements 
#' \item{x.t}{transformed 
#'   original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
#'   \item{n}{number of nonmissing observations}
#'   \item{norm_stat}{Pearson's P / degrees of freedom}
#'   \item{standardize}{was the transformation standardized}
#'   
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'   
#' @examples 
#' x <- rgamma(100, 1, 1)
#' 
#' exp_x_obj <- exp_x(x)
#' exp_x_obj
#' p <- predict(exp_x_obj)
#' x2 <- predict(exp_x_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @importFrom stats sd
#' @export
exp_x <- function(x, standardize = TRUE, warn = TRUE) {
  stopifnot(is.numeric(x))
  
  x.t <- exp(x)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  infinite_idx <- is.infinite(x.t)
  if (standardize) x.t <- (x.t - mu) / sigma

  if(any(infinite_idx)) {
    stop("infinite post-transformation values")
  }
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  class(val) <- c('exp_x', class(val))
  val
}

#' @rdname exp_x
#' @method predict exp_x
#' @export
predict.exp_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    newdata <-  log(newdata)
  } else if (!inverse) {
    newdata <- exp(newdata)
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname exp_x
#' @method print exp_x
#' @export
print.exp_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'exp(x) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


