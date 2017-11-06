#' Lambert W x F Normalization
#' 
#' @name lambert
#' @aliases predict.lambert
#'   
#' @description Perform Lambert's W x F transformation and center/scale a vector
#'   to attempt normalization via the \code{LambertW} package
#' @param x A vector to normalize with Box-Cox
#' @param type a character indicating which transformation to perform (options 
#'   are "s", "h", and "hh", see details)
#' @param ... Additional arguments that can be passed to the 
#'   LambertW::Gaussianize function
#'   
#' @details \code{lambert} uses the \code{LambertW} package to estimate a 
#'   normalizing (or "Gaussianizing") transformation. This transformation can be
#'   performed on new data, and inverted, via the \code{predict} function.
#'   
#'   NOTE: The type = "s" argument is the only one that does the 1-1 transform 
#'   consistently. Use type = "h" or type = 'hh' at risk of not having this 1-1 
#'   transform.
#'   
#' @return A list of class \code{lambert} with elements 
#'   \item{x.t}{transformed original data} 
#'   \item{x}{original data} 
#'   \item{mean}{mean of vector} 
#'   \item{sd}{sd of vector post-BC transformation} 
#'   \item{tau.mat}{estimated parameters of LambertW::Gaussianize} 
#'   \item{n}{size of vector}
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
#' x <- rgamma(100, 1, 1)
#' 
#' lambert_obj <- lambert(x)
#' lambert_obj
#' p <- predict(lambert_obj)
#' x2 <- predict(lambert_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @seealso  \code{\link[LambertW]{LambertW::Gaussianize}}
#' @export
lambert <- function(x, type = 's', ...) {
  obj <- unname(LambertW::Gaussianize(x, type = type, return.tau.mat = T, ...))
  x.t <- obj[[1]]
  tau.mat <- obj[[2]]
  
  mu <- mean(x.t)
  sigma <- sd(x.t)
  
  x.t <- (x.t - mu) / sigma
  
  attributes(x.t) <- NULL
  
  val <- list(
    x.t = unname(x.t),
    x = x,
    mean = mu,
    sd = sigma,
    tau.mat = tau.mat,
    n = length(x.t),
    type = type
  )
  
  class(val) <- 'lambert'
  val
}

#' @rdname lambert
#' @method predict lambert
#' @export
predict.lambert <- function(lambert_obj,
                            newdata = NULL,
                            inverse = FALSE) {
  if (is.null(newdata) & !inverse)
    newdata <- lambert_obj$x
  if (is.null(newdata) & inverse)
    newdata <- lambert_obj$x.t
  
  if (inverse)
    newdata <- newdata * lambert_obj$sd + lambert_obj$mean
  
  newdata <- LambertW::Gaussianize(
    as.matrix(newdata),
    type = lambert_obj$type,
    tau.mat = lambert_obj$tau.mat,
    inverse = inverse
  )
  
  if (!inverse)
    newdata <- (newdata - lambert_obj$mean) / lambert_obj$sd
  attributes(newdata) <- NULL
  
  unname(newdata)
}

#' @rdname lambert
#' @method print lambert
#' @export
print.lambert <- function(lambert_obj) {
  prettyTau <- apply(cbind('- ',
    rownames(lambert_obj$tau.mat)[-c(1:2)], ' = ',
    round(lambert_obj$tau.mat[-c(1:2)],4), '\n'
  ), 1, paste, collapse = '')

  
  cat('Lambert WxF Transformation of type', lambert_obj$type, 
      'with', lambert_obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      prettyTau,
      '- mean =', lambert_obj$mean, '\n',
      '- sd =', lambert_obj$sd, '\n')
}
