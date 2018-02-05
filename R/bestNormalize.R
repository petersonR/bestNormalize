#' Calculate and perform best normalizing transformation
#' 
#' @aliases predict.bestNormalize
#'   
#' @description Performs a suite of normalizing transformations, and selects the
#'   best one on the basis of the Pearson P test statistic for normality. The 
#'   transformation that has the lowest P (calculated on the transformed data) 
#'   is selected. See details for more information.
#'   
#' @param x A vector to normalize
#' @param allow_orderNorm set to FALSE if orderNorm should not be applied
#' @param out_of_sample if FALSE, estimates quickly in-sample performance
#' @param cluster name of cluster set using \code{makeCluster}
#' @param k number of folds
#' @param r number of repeats
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
#' \item{norm_stats}{Pearson's Pearson's P / degrees of freedom}
#' \item{method}{out-of-sample or in-sample, number of folds + repeats}
#' \item{chosen_transform}{the chosen transformation (of appropriate class)}
#' \item{other_transforms}{the other transformations (of appropriate class)}
#' 
#' The \code{predict} function returns the numeric value of the transformation
#' performed on new data, and allows for the inverse transformation as well.
#' 
#' @details This function currently estimates the Yeo-Johnson transformation,
#'   the Box Cox transformation (if the data is positive), and the Lambert WxF
#'   Gaussianizing transformation of type "s". If allow_orderNorm == TRUE and if
#'   out_of_sample == FALSE then the ordered quantile normalization technique
#'   will likely be chosen since it essentially forces the data to follow a
#'   normal distribution. More information on the orderNorm technique can be
#'   found in the package vignette, or using \code{?orderNorm}.
#'
#'   Repeated cross-validation is used to estimate the out-of-sample performance
#'   of each transformation if out_of_sample = TRUE. While this can take some
#'   time, users can speed it up by creating a cluster via the \code{parallel}
#'   package's \code{makeCluster} function, and passing the name of this cluster
#'   to \code{bestNormalize} via the cl argument. For best performance, we
#'   recommend the number of clusters to be set to the number of repeats r. Care
#'   should be taken to account for the number of observations per fold; to
#'   small a number and the estimated normality statistic could be inaccurate,
#'   or at least suffer from high variability.
#'
#'   NOTE: Only the Lambert technique of type = "s" (skew) ensures that the
#'   transformation is consistently 1-1, so it is the only method currently used
#'   in \code{bestNormalize()}. Use type = "h" or type = 'hh' at risk of not
#'   having this estimate 1-1 transform. These alternative types are effective
#'   when the data has exceptionally heavy tails, e.g. the Cauchy distribution.
#' 
#' @examples 
#' 
#' x <- rgamma(100, 1, 1)
#' 
#' \dontrun{
#' # With Repeated CV
#' BN_obj <- bestNormalize(x) 
#' BN_obj
#' p <- predict(BN_obj)
#' x2 <- predict(BN_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' }
#' 
#' # Without Repeated CV
#' BN_obj <- bestNormalize(x, allow_orderNorm = FALSE, out_of_sample = FALSE) 
#' BN_obj
#' p <- predict(BN_obj)
#' x2 <- predict(BN_obj, newdata = p, inverse = TRUE)
#' 
#' all.equal(x2, x)
#' 
#' 
#' @seealso  \code{\link[bestNormalize]{boxcox}},
#'  \code{\link{lambert}}, 
#'  \code{\link{orderNorm}},
#'  \code{\link{yeojohnson}} 
#' @export
bestNormalize <- function(x, allow_orderNorm = TRUE, out_of_sample = TRUE, 
                          cluster = NULL, k = 10, r = 5) {
  stopifnot(is.numeric(x))
  x.t <- list()
  methods <- c('lambert', 'yeojohnson', 'boxcox')
  if (allow_orderNorm) 
    methods <- c(methods, 'orderNorm')
  
  for(i in methods) {
    trans_i <- try(do.call(i, list(x = x)), silent = TRUE)
    if(is.character(trans_i))
      warning(paste(i, ' did not work; ', trans_i))
    else
      x.t[[i]] <- trans_i
  }

  # Select methods that worked
  methods <- names(x.t)
  
  ## Estimate out-of-sample P/df
  if(out_of_sample) {
    k <- as.integer(k)
    r <- as.integer(r)
    reps <- get_oos_estimates(x, methods, k, r, cluster)
    norm_stats <- colMeans(reps)
    best_idx <- names(which.min(norm_stats))
    method <- paste("Out-of-sample via CV with", k, "folds and", r, "repeats")
  } else {
    norm_stats <- unlist(lapply(x.t, function(x) x$norm_stat))
    best_idx <- names(which.min(norm_stats))
    method <- "In-sample"
    reps <- NULL
  }

  
  val <- list(
    x.t = x.t[[best_idx]]$x.t,
    x = x,
    norm_stats = norm_stats,
    method = method,
    resampled_norm_stats = reps,
    chosen_transform = x.t[[best_idx]],
    other_transforms = x.t[names(x.t) != best_idx]
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
           paste(" - orderNorm:", round(x$norm_stats['orderNorm'], 4), '\n'), ''), 
    "Estimation method: ", x$method, '\n')
  
  cat('Best Normalizing transformation with', x$chosen_transform$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(x$chosen_transform)
}

#' @importFrom parallel parLapplyLB clusterExport
get_oos_estimates <- function(x, norm_methods, k, r, cluster) {
  x <- x[!is.na(x)]
  fold_size <- floor(length(x) / k)
  if(fold_size < 20) warning("fold_size is ", fold_size, " (< 20), therefore P/df estimates may be off") 
  
  # Perform in this session is cluster unspecified
  if(is.null(cluster)) {
    reps <- lapply(1:r, function(rep) {
      
      resamples <- create_folds(x, k)
      pstats <- matrix(NA, ncol = length(norm_methods), nrow = k)
      for(i in 1:k) {
        for(m in 1:length(norm_methods)) {
          trans_m <- suppressWarnings(try(do.call(norm_methods[m], list(x = x[resamples != i])), silent = TRUE))
          if(is.character(trans_m)) {
            stop(paste(norm_methods[m], ' did not work; ', trans_m))
            pstats[i, m] <- NA
          } else {
            vec <- suppressWarnings(predict(trans_m, newdata = x[resamples == i]))
            p <- nortest::pearson.test(vec)
            pstats[i, m] <- p$stat / p$df
          }
        }
      }
      colnames(pstats) <- norm_methods
      rownames(pstats) <- paste0("Rep", rep, "Fold", 1:k)
      pstats
    })
    reps <- Reduce(rbind, reps)
  } else {
    
    clusterExport(cl = cluster, c("k", "x", "norm_methods", norm_methods), envir = environment())
    reps <- parLapplyLB(cl = cluster, 1:r, function(rep) {
      
      resamples <- create_folds(x, k)
      pstats <- matrix(NA, ncol = length(norm_methods), nrow = k)
      for(i in 1:k) {
        for(m in 1:length(norm_methods)) {
          trans_m <-  suppressWarnings(try(do.call(norm_methods[m], list(x = x[resamples != i])), silent = TRUE))
          if(is.character(trans_m)) {
            stop(paste(norm_methods[m], ' did not work; ', trans_m))
            pstats[i, m] <- NA
          } else {
            vec <- suppressWarnings(predict(trans_m, newdata = x[resamples == i]))
            p <- nortest::pearson.test(vec)
            pstats[i, m] <- p$stat / p$df
          }
        }
      }
      colnames(pstats) <- norm_methods
      rownames(pstats) <- paste0("Rep", rep, "Fold", 1:k)
      pstats
    })
    reps <- Reduce(rbind, reps)
  }
  reps
}

create_folds <- function(x, k) { 
  as.numeric(cut(sample(1:length(x)), breaks = k))
}
