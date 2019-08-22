#' sqrt(x + a) Normalization
#'
#' @name sqrt_x
#' @aliases predict.sqrt_x
#'
#' @description Perform a sqrt (x+a) normalization transformation
#' @param x A vector to normalize with with x
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param a The constant to add to x (defaults to max(0, -min(x)))
#' @param object an object of class 'sqrt_x'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{sqrt_x} performs a simple square-root transformation in the
#'   context of bestNormalize, such that it creates a transformation that can be
#'   estimated and applied to new data via the \code{predict} function. The
#'   parameter a is essentially estimated by the training set by default
#'   (estimated as the minimum possible), while the base
#'   must be specified beforehand.
#'
#' @return A list of class \code{sqrt_x} with elements \item{x.t}{transformed
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
#' sqrt_x_obj <- sqrt_x(x)
#' sqrt_x_obj
#' p <- predict(sqrt_x_obj)
#' x2 <- predict(sqrt_x_obj, newdata = p, inverse = TRUE)
#'
#' all.equal(x2, x)
#'
#' @importFrom stats sd
#' @export
sqrt_x <- function(x, a = NULL, standardize = TRUE) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE)))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x))) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
  
  
  x.t <- sqrt(x + a)
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  if (standardize) x.t <- (x.t - mu) / sigma
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    a = a,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  class(val) <- c('sqrt_x', class(val))
  val
}

#' @rdname sqrt_x
#' @method predict sqrt_x
#' @export
predict.sqrt_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    newdata <-  newdata^2 - object$a
  } else if (!inverse) {
    newdata <- sqrt(newdata + object$a)
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname sqrt_x
#' @method print sqrt_x
#' @export
print.sqrt_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'sqrt(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


