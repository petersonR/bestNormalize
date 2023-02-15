#' Double Reverse Log(x + a) Transformation
#' 
#' @name double_reverse_log
#' @aliases predict.double_reverse_log
#'   
#' @description First reverses scores, then perform a log_b(x) 
#' normalization transformation, and then reverses scores again.
#' 
#' @param x A vector to normalize with with x
#' @param b The base of the log (defaults to 10)
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param eps The cushion for the transformation range (defaults to 10 percent)
#' @param warn Should a warning result from infinite values?
#' @param object an object of class 'double_reverse_log'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{double_reverse_log} performs a simple log transformation in the 
#' context of bestNormalize, such that it creates a transformation that can be 
#' estimated and applied to new data via the \code{predict} function. The parameter 
#' a is essentially estimated by the training set by default (estimated as the 
#' minimum possible to some extent epsilon), while the base must be specified 
#' beforehand.
#'  
#' @return A list of class \code{double_reverse_log} with elements 
#' \item{x.t}{transformed 
#'   original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
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
#' double_reverse_log_obj <- double_reverse_log(x)
#' double_reverse_log_obj
#' p <- predict(double_reverse_log_obj)
#' x2 <- predict(double_reverse_log_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @importFrom stats sd
#' @export
double_reverse_log  <- function(x, 
                                b = 10, 
                                standardize = TRUE, 
                                eps = diff(range(x, na.rm = TRUE))/10,
                                warn = TRUE,
                                ...) {
  stopifnot(is.numeric(x))
  
  # Calculated padded max + min of x
  max_x <- max(x, na.rm = TRUE) + eps
  min_x <- min(x, na.rm = TRUE) - eps
  
  # calculate padded max(x.t)
  max_xt <- log(max_x - min_x, base = b)
  
  # Perform transformation (reverse, log, reverse)
  x_rev <- max_x - x
  x.t_rev <- log(x_rev, base = b) 
  x.t <- max_xt - x.t_rev 
  
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
    b = b,
    eps = eps,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize,
    max_x = max_x,
    min_x = min_x,
    max_xt = max_xt
  )
  class(val) <- c('double_reverse_log', class(val))
  val
  
}

#' @rdname double_reverse_log
#' @method predict double_reverse_log
#' @export
predict.double_reverse_log <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse){
    newdata <- object$x.t
  }
  if (inverse) {
    if (object$standardize) {
      newdata <- newdata * object$sd + object$mean
    }
    
    # Perform transformation
    new_xt_rev <- object$max_xt - newdata
    new_x_rev <- object$b^new_xt_rev
    newdata <- object$max_x - new_x_rev

    } else if (!inverse) {
      
      new_x_rev <- object$max_x - newdata 
      new_xt_rev <-  log(new_x_rev, object$b)
      newdata <- object$max_xt - new_xt_rev 
      
    if (object$standardize) {
      newdata <- (newdata - object$mean) / object$sd
    }
  }
  
  unname(newdata)
}

#' @rdname double_reverse_log
#' @method print double_reverse_log
#' @export
print.double_reverse_log <- function(x, ...) {
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'double reversed Log_b(x + a) Transformation with', x$n, 'nonmissing obs.:\n', 
      'Relevant statistics:\n',
      '- a =', x$a, '\n',
      '- b =', x$b, '\n',
      '- max(x) =', x$max_x, '; min(x) =', x$min_x, '\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


