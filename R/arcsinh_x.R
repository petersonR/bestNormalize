#' arcsinh(x) Transformation
#' 
#' @name arcsinh_x
#' @aliases predict.arcsinh_x
#'   
#' @description Perform a arcsinh(x) transformation 
#' @param x A vector to normalize with with x
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param object an object of class 'arcsinh_x'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{arcsinh_x} performs an arcsinh transformation in the context of 
#' bestNormalize, such that it creates a transformation that can be estimated
#' and applied to new data via the \code{predict} function. 
#' 
#' The function is explicitly: log(x + sqrt(x^2 + 1))
#'  
#' @return A list of class \code{arcsinh_x} with elements 
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
#' arcsinh_x_obj <- arcsinh_x(x)
#' arcsinh_x_obj
#' p <- predict(arcsinh_x_obj)
#' x2 <- predict(arcsinh_x_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @importFrom stats sd
#' @export
arcsinh_x <- function(x, standardize = TRUE) {
  stopifnot(is.numeric(x))
  
  x.t <- asinh(x)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
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
  class(val) <- c('arcsinh_x', class(val))
  val
}

#' @rdname arcsinh_x
#' @method predict arcsinh_x
#' @export
predict.arcsinh_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    newdata <-  sinh(newdata)
  } else if (!inverse) {
    newdata <- asinh(newdata)
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname arcsinh_x
#' @method print arcsinh_x
#' @export
print.arcsinh_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'asinh(x) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


