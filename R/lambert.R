#' Lambert W x F Normalization
#' 
#' @name lambert
#' @aliases predict.lambert
#'   
#' @description Perform Lambert's W x F transformation and center/scale a vector
#'   to attempt normalization via the \code{LambertW} package.
#' @param x A vector to normalize with Box-Cox
#' @param type a character indicating which transformation to perform (options 
#'   are "s", "h", and "hh", see details)
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal
#' @param ... Additional arguments that can be passed to the 
#'   LambertW::Gaussianize function
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'lambert'
#' 
#' @details \code{lambert} uses the \code{LambertW} package to estimate a 
#'   normalizing (or "Gaussianizing") transformation. This transformation can be
#'   performed on new data, and inverted, via the \code{predict} function.
#'   
#'   NOTE: The type = "s" argument is the only one that does the 1-1 transform
#'   consistently, and so it is the only method currently used in
#'   \code{bestNormalize()}. Use type = "h" or type = 'hh' at risk of not having
#'   this estimate 1-1 transform. These alternative types are effective when the
#'   data has exceptionally heavy tails, e.g. the Cauchy distribution.
#'   
#'   Additionally, sometimes (depending on the distribution) this method will be
#'   unable to extrapolate beyond the observed bounds. In these cases, NaN is
#'   returned.
#'   
#' @return A list of class \code{lambert} with elements 
#'   \item{x.t}{transformed original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean after transformation but prior to standardization} 
#'   \item{sd}{sd after transformation but prior to standardization} 
#'   \item{tau.mat}{estimated parameters of LambertW::Gaussianize} 
#'   \item{n}{number of nonmissing observations}
#'   \item{norm_stat}{Pearson's P / degrees of freedom}
#'   \item{standardize}{was the transformation standardized}
#'   
#' The \code{predict} function returns the numeric value of the transformation 
#' performed on new data, and allows for the inverse transformation as well.
#' 
#' @references Georg M. Goerg (2016). LambertW: An R package for Lambert W x F 
#'   Random Variables. R package version 0.6.4.
#'   
#'   Georg M. Goerg (2011): Lambert W random variables - a new family of 
#'   generalized skewed distributions with applications to risk estimation. 
#'   Annals of Applied Statistics 3(5). 2197-2230.
#'   
#'   Georg M. Goerg (2014): The Lambert Way to Gaussianize heavy-tailed data 
#'   with the inverse of Tukey's h transformation as a special case. The 
#'   Scientific World Journal.
#'   
#' @examples 
#' \dontrun{
#' x <- rgamma(100, 1, 1)
#' 
#' lambert_obj <- lambert(x)
#' lambert_obj
#' p <- predict(lambert_obj)
#' x2 <- predict(lambert_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' }
#' 
#' @seealso  \code{\link[LambertW]{Gaussianize}}
#' @importFrom stats sd
#' @export
lambert <- function(x, type = 's', standardize = TRUE, ...) {
  stopifnot(is.numeric(x))
  na_idx <- is.na(x)
  x_complete <- x[!na_idx]
  obj <- unname(LambertW::Gaussianize(x_complete, type = type, return.tau.mat = T, ...))
  
  x.t <- x
  x.t[!na_idx] <- obj[[1]]
  tau.mat <- obj[[2]]
  
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  
  if (standardize) x.t <- (x.t - mu) / sigma
  
  attributes(x.t) <- NULL
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = unname(x.t),
    x = x,
    mean = mu,
    sd = sigma,
    tau.mat = tau.mat,
    n = length(x.t) - sum(na_idx),
    type = type,
    norm_stat = unname(ptest$statistic / ptest$df),
    standardize = standardize
  )
  
  class(val) <- 'lambert'
  val
}

#' @rdname lambert
#' @method predict lambert
#' @export
predict.lambert <- function(object,
                            newdata = NULL,
                            inverse = FALSE, 
                            ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse & object$standardize)
    newdata <- newdata * object$sd + object$mean
  
  stopifnot(is.numeric(newdata))
  
  na_idx <- is.na(newdata)

  newdata[!na_idx] <- LambertW::Gaussianize(
    as.matrix(newdata[!na_idx]),
    type = object$type,
    tau.mat = object$tau.mat,
    inverse = inverse
  )
  
  if (!inverse & object$standardize)
    newdata <- (newdata - object$mean) / object$sd
  
  attributes(newdata) <- NULL
  unname(newdata)
}

#' @rdname lambert
#' @method print lambert
#' @export
print.lambert <- function(x, ...) {
  prettyTau <- apply(cbind('- ',
    rownames(x$tau.mat)[-c(1:2)], ' = ',
    round(x$tau.mat[-c(1:2)],4), '\n'
  ), 1, paste, collapse = '')

  
  cat(ifelse(x$standardize, "Standardized", "Non-Standardized"),
      'Lambert WxF Transformation of type', x$type, 
      'with', x$n, 'nonmissing obs.:\n', 
      'Estimated statistics:\n',
      prettyTau,
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}
