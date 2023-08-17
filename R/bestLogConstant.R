#' Calculate and perform best normalizing log transformation (experimental)
#'
#' @aliases predict.bestLogConstant 
#'
#' @description Similar to bestNormalize, this selects the
#'   best candidate constant for a log transformation on the basis 
#'   of the Pearson P test statistic for normality. The
#'   transformation that has the lowest P (calculated on the transformed data)
#'   is selected. This function is currently in development and may not behave 
#'   as expected. 
#'   
#'   See details for more information.
#'
#' @param x A vector to normalize
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal. This will
#'   not change the normality statistic.
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'bestLogConstant'
#' @param a (optional) a list of candidate constants to choose from
#' @param ... additional arguments.

#' @details \code{bestLogConstant} estimates the optimal normalizing constant 
#'   for a log transformation. This transformation can be performed on new data, and
#'   inverted, via the \code{predict} function.
#'
#' @return A list of class \code{bestLogConstant} with elements
#'
#'   \item{x.t}{transformed original data} \item{x}{original data}
#'   \item{norm_stats}{Pearson's Pearson's P / degrees of freedom}
#'   \item{method}{out-of-sample or in-sample, number of folds + repeats}
#'   \item{chosen_constant}{the chosen constant transformation (of class `log_x`)}
#'   \item{other_transforms}{the other transformations (of class `log_x`)}
#'
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'
#'
#' @seealso  \code{\link[bestNormalize]{bestNormalize}}, \code{\link{log_x}},
#' @export
bestLogConstant <- function(x, 
                            a,
                            standardize = TRUE, 
                          ...) {
  stopifnot(is.numeric(x))
  x.t <- list()

  if(missing(a))
    a = c(10^(-2:9))
  
  args <- lapply(a, function(x) list("a"= x, "standardize" = standardize))
  
  method_calls <- rep("log_x", length = length(a))
  names(args) <- method_calls
  
  
  for(i in 1:length(method_calls)) {
    args_i <- args[[i]]
    args_i$x <- x
    
    trans_i <- try(do.call(method_calls[i], args_i), silent = TRUE)
    x.t[[i]] <- trans_i
  }
  
  norm_stat_fn <- function(x) {
    val <- nortest::pearson.test(x)
    unname(val$stat/val$df)
  }

  norm_stats <- unlist(lapply(x.t, function(x) norm_stat_fn(x$x.t)))
  names(norm_stats) <- paste0("a=",a)
  best_idx <- which.min(norm_stats)
  
  val <- list(
    x.t = x.t[[best_idx]]$x.t,
    x = x,
    a = a,
    best_a = a[best_idx],
    norm_stats = norm_stats,
    chosen_transform = x.t[[best_idx]],
    other_transforms = x.t[names(x.t) != best_idx],
    standardize = standardize
  )
  class(val) <- 'bestLogConstant'
  val
}

#' @rdname bestLogConstant
#' @method predict bestLogConstant
#' @importFrom stats predict
#' @export
predict.bestLogConstant <- function(object, newdata = NULL, inverse = FALSE, ...) {
  predict(object$chosen_transform, newdata = newdata, inverse = inverse, ...)
}

#' @rdname bestLogConstant
#' @method print bestLogConstant
#' @export
print.bestLogConstant <- function(x, ...) {
  
  results <- paste0(" - ", x$a, ": ", round(x$norm_stats, 4), collapse="\n")
  
  prettyD <- paste0(
    'Estimated Normality Statistics\n',
    "(Pearson P / df, lower => more normal):\n",
    results, '\n')
  
  cat('Best Normalizing log constant transformation with', x$chosen_transform$n, 'Observations\n',
      prettyD, '\nBased off these, bestLogConstant chose:\n')
  print(x$chosen_transform)
}
