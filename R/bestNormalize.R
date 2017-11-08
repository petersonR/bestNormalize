#' Calculate and perform best normalizing tranformation
#' 
#' @aliases predict.bestNormalize
#'   
#' @description Performs a suite of normalizing transformations, and selects the
#'   best one on the basis of the Shapiro-Wilks test for normality (ie, the one 
#'   with the highest p-value, ie the one that shows the least amount of
#'   evidence against normality).
#' @param x A vector to normalize
#' @param allow_orderNorm set to FALSE if orderNorm should not be applied
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'bestNormalize'
#' @param ... additional arguments
#' @details \code{bestNormalize} estimates the optimal normalizing 
#'   transformation. This transformation can be performed on new data, and 
#'   inverted, via the \code{predict} function.
#'   
#' @return A list of class \code{bestNormalize} with elements 
#' 
#' \item{x.t}{transformed original data} 
#' \item{x}{original data} 
#' \item{norm_stats}{Pearson's chi-squared normality test statistics}
#' \item{chosen_transform}{info about the tranformation (of appropriate class)}
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
bestNormalize <- function(x, allow_orderNorm = TRUE) {
  stopifnot(is.numeric(x))
  x.t <- list()
  methods <- c('lambert', 'yeojohnson', 'boxcox', 'binarize')
  if (allow_orderNorm) 
    methods <- c(methods, 'orderNorm')
  
  for(i in methods) {
    trans_i <- try(do.call(i, list(x = x)), silent = TRUE)
    if(is.character(trans_i))
      warning(paste(i, ' did not work; ', trans_i))
    else
      x.t[[i]] <- trans_i
  }

  norm_stats <- unlist(lapply(x.t, function(x) x$norm_stat))
  best_idx <- names(which.min(norm_stats))
  
  val <- list(
    x.t = x.t[[best_idx]]$x.t,
    x = x,
    norm_stats = norm_stats,
    chosen_transform = x.t[[best_idx]]
  )
  class(val) <- 'bestNormalize'
  val
}

#' @rdname bestNormalize
#' @method predict bestNormalize
#' @importFrom stats predict
#' @export
predict.bestNormalize <- function(object, newdata = NULL, inverse = FALSE, ...) {
  predict(object$chosen_transform, newdata = newdata, inverse = inverse, ...)
}

#' @rdname bestNormalize
#' @method print bestNormalize
#' @export
print.bestNormalize <- function(x, ...) {
  prettyD <- paste0(
    'Estimated Normality Statistics (Pearson P / df, lower => more normal):\n',
    ifelse(length(x$norm_stats['boxcox']), 
           paste(" - Box-Cox:", round(x$norm_stats['boxcox'], 4), '\n'), ''),
    ifelse(length(x$norm_stats['lambert']), 
           paste(" - Lambert's W:", round(x$norm_stats['lambert'], 4), '\n'), ''),
    ifelse(length(x$norm_stats['yeojohnson']), 
           paste(" - Yeo-Johnson:", round(x$norm_stats['yeojohnson'], 4), '\n'), ''),
    ifelse(length(x$norm_stats['orderNorm']), 
           paste(" - orderNorm:", round(x$norm_stats['orderNorm'], 4), '\n'), '')
  )
  
  cat('Best Normalizing transformation with', x$chosen_transform$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(x$chosen_transform)
}
