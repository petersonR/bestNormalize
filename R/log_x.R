#' Log(x + a) Transformation
#' 
#' @name log_x
#' @aliases predict.log_x
#'   
#' @description Perform a log_b (x+a) normalization transformation 
#' @param x A vector to normalize with with x
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param a The constant to add to x (defaults to max(0, -min(x) + eps))
#' @param b The base of the log (defaults to 10)
#' @param eps The allowed error in the expression for the selected a
#' @param warn Should a warning result from infinite values?
#' @param object an object of class 'log_x'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{log_x} performs a simple log transformation in the context of 
#' bestNormalize, such that it creates a transformation that can be estimated
#' and applied to new data via the \code{predict} function. The parameter a is
#' essentially estimated by the training set by default (estimated as the minimum
#' possible to some extent epsilon), while the base must be 
#' specified beforehand.
#'  
#' @return A list of class \code{log_x} with elements 
#' \item{x.t}{transformed 
#'   original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
#'   \item{a}{estimated a value} 
#'   \item{b}{estimated base b value} 
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
#' log_x_obj <- log_x(x)
#' log_x_obj
#' p <- predict(log_x_obj)
#' x2 <- predict(log_x_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @importFrom stats sd
#' @export
log_x <- function(x, a = NULL, b = 10, standardize = TRUE, eps = .001, warn = TRUE) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE) - eps))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x) - eps)) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
   
  
  x.t <- log(x + a, base = b)
  
  stopifnot(!all(infinite_idx <- is.infinite(x.t)))
  if(any(infinite_idx)) {
    warning("Some values are infinite")
    standardize <- FALSE
  }
  
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
    b = b,
    eps = eps,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  class(val) <- c('log_x', class(val))
  val
}

#' @rdname log_x
#' @method predict log_x
#' @export
predict.log_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) 
      newdata <- newdata * object$sd + object$mean
    newdata <-  object$b^newdata - object$a
  } else if (!inverse) {
    newdata <- log(newdata + object$a, object$b)
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname log_x
#' @method print log_x
#' @export
print.log_x <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'Log_b(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- b =', x$b, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


