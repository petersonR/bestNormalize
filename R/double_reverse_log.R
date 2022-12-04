#' Double Reverse Log(x + a) Transformation
#' 
#' @name double_reverse_log
#' @aliases predict.double_reverse_log
#'   
#' @description First reverse scores, then perform a log_b (x+a) 
#' normalization transformation, and then reverse scores again.
#' @param x A vector to normalize with with x
#' @param a The constant to add to x (defaults to max(0, -min(x) + eps))
#' @param b The base of the log (defaults to 10)
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param eps The allowed error in the expression for the selected a
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
                                a = NULL, 
                                b = 10, 
                                standardize = TRUE, 
                                eps = .001, 
                                warn = TRUE,
                                ...) {
  stopifnot(is.numeric(x))
  
  min_a <- max(0, -(min(x, na.rm = TRUE) - eps))
  if(!length(a)) 
    a <- min_a
  if(a < min_a) {
    warning("Setting a <  max(0, -(min(x) - eps)) can lead to transformation issues",
            "Standardize set to FALSE")
    standardize <- FALSE
  }
  
  x.t <- nice_reverse(x, max = max(x, na.rm = TRUE), min = min(x, na.rm = TRUE))
  x.t <- log(x.t + a, base = b)
  x.t <- nice_reverse(x.t, max = max(x.t, na.rm = TRUE), min = min(x.t, na.rm = TRUE))
  
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
    standardize = standardize,
    max_x = max(x, na.rm = TRUE),
    min_x = min(x, na.rm = TRUE)
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
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    if (object$standardize) {
      newdata <- newdata * object$sd + object$mean
    }

    newdata <- nice_reverse(newdata,
                            max = max(newdata, na.rm = TRUE),
                            min = min(newdata, na.rm = TRUE))
    newdata <-  object$b^newdata - object$a
    newdata <- nice_reverse(newdata,
                            max = max(newdata, na.rm = TRUE),
                            min = min(newdata, na.rm = TRUE))

    } else if (!inverse) {
    
    newdata <- nice_reverse(newdata, 
                            max = max(newdata, na.rm = TRUE), 
                            min = min(newdata, na.rm = TRUE))
    newdata <- log(newdata + object$a, object$b)
    newdata <- nice_reverse(newdata, 
                            max = max(newdata, na.rm = TRUE),
                            min = min(newdata, na.rm = TRUE))
    
    if (object$standardize) 
      newdata <- (newdata - object$mean) / object$sd
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
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}


