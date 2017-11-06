#' Calculate and perform best normalizing tranformation
#' 
#' @name bestNormalize
#' @aliases predict.bestNormalize
#'   
#' @description Performs a suite of normalizing transformations, and selects the
#'   best one on the basis of a particular statistic.
#' @param x A vector to normalize
#' @details \code{bestNormalize} estimates the optimal normalizing
#'   transformation. This transformation can be performed on new data, and 
#'   inverted, via the \code{predict} function.
#'   
#' @return A list of class \code{bestNormalize} with elements 
#' 
#' \item{x.t}{transformed original data} 
#' \item{x}{original data} 
#' \item{method}{method of normalization chosen}
#' \item{chosen_transform}{info about the tranformation (of appropriate class)}
#' \item{n}{size of vector}
#'   
#' The \code{predict} function returns the numeric value of the transformation
#' performed on new data, and allows for the inverse transformation as well.
#' 
#' @examples 
#' 
#' x <- rgamma(100, 1, 1)
#' 
#' BN_obj <- bestNormalize(x)
#' BN_obj
#' p <- predict(BN_obj)
#' x2 <- predict(BN_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' @seealso  \code{\link[bestNormalize]{boxcox}},
#'  \code{\link{lambert}}, 
#'  \code{\link{orderNorm}},
#'  \code{\link{yeojohnson}} 
#' @export
bestNormalize <- function(x,
                          D_max = 1,
                          allow_orderNorm = T,
                          ...) {
  
  x.t <- list()
  x.t[['lambert']] <- lambert(x)
  x.t[['yeojohnson']] <- yeojohnson(x)
  
  if (all(x > 0)) x.t[['boxcox']] <- boxcox(x)
  
  D <- unlist(lapply(x.t, function(x)
    unname(nortest::lillie.test(as.vector(x$x.t))$statistic)))

  if (min(D, na.rm = T) > D_max) {
    if (allow_orderNorm) {
      x.t$orderNorm <- orderNorm(x, ...)
      D <- c(D, nortest::lillie.test(as.vector(x.t$orderNorm$x.t))$statistic)
      names(D)[length(D)] <- 'orderNorm'
    } else {
      x.t$binarize <- binarize(x, ...)
      D <- c(D, -Inf)
      names(D)[length(D)] <- 'binarize'
      warning('Data binarized, consider increasing D_max or allowing orderNorm')
    }
  }
  
  xFinal <- x.t[[which.min(D)]]$x.t
  val <- list(
    x.t = xFinal,
    x = x,
    D = D,
    n = length(xFinal),
    chosen_transform = x.t[[which.min(D)]]
  )
  class(val) <- 'bestNormalize'
  val
}

#' @rdname bestNormalize
#' @method predict bestNormalize
#' @export
predict.bestNormalize <- function(BNobject, newdata = NULL, inverse = FALSE) {
  obj <- BNobject$chosen_transform
  predict(obj, newdata = newdata, inverse = inverse)
}

#' @rdname bestNormalize
#' @method print bestNormalize
#' @export
print.bestNormalize <- function(BNobject) {
  
  prettyD <- paste0(
    'Estimated Normality Statistics (D):\n',
    ifelse(length(BNobject$D['boxcox']), 
           paste(" - Box-Cox:", round(BNobject$D['boxcox'], 4), '\n'), ''),
    ifelse(length(BNobject$D['boxcox']), 
           paste(" - Lambert's W:", round(BNobject$D['lambert'], 4), '\n'), ''),
    ifelse(length(BNobject$D['lambert']), 
           paste(" - Yeo-Johnson:", round(BNobject$D['yeojohnson'], 4), '\n'), '')
  )
  
  cat('Best Normalizing transformation with', BNobject$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(BNobject$chosen_transform)
}
