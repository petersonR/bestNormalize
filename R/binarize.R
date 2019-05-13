#' Binarize
#' 
#' @name binarize
#' @aliases predict.binarize
#'   
#' @description This function will perform a binarizing transformation, which
#'   could be used as a last resort if the data cannot be adequately normalized.
#'   This may be useful when accidentally attempting normalization of a binary
#'   vector (which could occur if implementing bestNormalize in an automated
#'   fashion).
#'   
#'   Note that the transformation is not one-to-one, in contrast to the other
#'   functions in this package.
#'   
#' @param x A vector to binarize
#' @param location_measure which location measure should be used? can either be
#'   "median", "mean", "mode", a number, or a function.
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'binarize'
#' @param ... additional arguments
#' 
#' @return A list of class \code{binarize} with elements 
#'   \item{x.t}{transformed original data} 
#'   \item{x}{original data} 
#'   \item{method}{location_measure used for original fitting} 
#'   \item{location}{estimated location_measure} 
#'   \item{n}{number of nonmissing observations}
#'   \item{norm_stat}{Pearson's P / degrees of freedom}
#'   
#'   The \code{predict} function with \code{inverse = FALSE} returns the numeric
#'   value (0 or 1) of the transformation on \code{newdata} (which defaults to
#'   the original data).
#'   
#'   If \code{inverse = TRUE}, since the transform is not 1-1, it will create
#'   and return a factor that indicates where the original data was cut.
#'   
#' @examples 
#' x <- rgamma(100, 1, 1)
#' binarize_obj <- binarize(x)
#' (p <- predict(binarize_obj))
#' 
#' predict(binarize_obj, newdata = p, inverse = TRUE)
#' @importFrom stats density median
#' @export
binarize <- function(x, location_measure = 'median') {
  stopifnot(is.numeric(x))
  
  # Check and set location measure
  if (is.numeric(location_measure)) {
    loc <- location_measure
  } else if (is.function(location_measure)) {
    loc <- location_measure(x)
  } else  if (location_measure == 'median') {
    loc <- median(x, na.rm = T)
  } else if (location_measure == 'mean') {
    loc <- mean(x, na.rm = T)
  } else if (location_measure == 'mode') {
    dens <- density(x[!is.na(x)])
    loc <- dens$x[which.max(dens$y)]
  }
  
  x.t <- as.numeric(x > loc)
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    method = location_measure,
    location = loc,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df)
  )
  class(val) <- 'binarize'
  val
}


#' @rdname binarize
#' @method predict binarize
#' @export
predict.binarize <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata))
    newdata <- object$x.t
  
  if (!inverse)
    return(as.numeric(newdata > object$location))
  prettyLoc <- round(object$location,
                     getOption('digits', 2))
  labels <- c(paste0('< ', prettyLoc),
              paste0('>= ', prettyLoc))
  factor(newdata, levels = 0:1, labels = labels)
}


#' @rdname binarize
#' @method print binarize
#' @export
print.binarize <- function(x, ...) {
  cat('Binarize Transformation with', x$n, 
      'nonmissing obs.\nEstimated Statistic:\n -', x$method,
      '=', x$location)
}

