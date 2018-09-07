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
#' @param allow_lambert_s Set to TRUE if the lambertW of type "s"  should be
#'   applied (see details)
#' @param allow_lambert_h Set to TRUE if the lambertW of type "h"  should be
#'   applied (see details)
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal. This will
#'   not change the normality statistic.
#' @param out_of_sample if FALSE, estimates quickly in-sample performance
#' @param cluster name of cluster set using \code{makeCluster}
#' @param k number of folds
#' @param r number of repeats
#' @param warn Should bestNormalize warn when a method doesn't work?
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
#'   \item{x.t}{transformed original data} \item{x}{original data}
#'   \item{norm_stats}{Pearson's Pearson's P / degrees of freedom}
#'   \item{method}{out-of-sample or in-sample, number of folds + repeats}
#'   \item{chosen_transform}{the chosen transformation (of appropriate class)}
#'   \item{other_transforms}{the other transformations (of appropriate class)}
#'
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'
#' @details This function currently estimates the Yeo-Johnson transformation,
#'   the Box Cox transformation (if the data is positive), the log_10(x+a)
#'   transformation, the square-root (x+a) transformation, and the arcsinh
#'   transformation. a is set to max(0, -min(x) + eps) by default.  If
#'   allow_orderNorm == TRUE and if out_of_sample == FALSE then the ordered
#'   quantile normalization technique will likely be chosen since it essentially
#'   forces the data to follow a normal distribution. More information on the
#'   orderNorm technique can be found in the package vignette, or using
#'   \code{?orderNorm}.
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
#'   Additionally, as of v. 1.2.0, Lambert of type "s" is not used by default in
#'   \code{bestNormalize()} since it uses multiple threads on some Linux systems,
#'   which is not allowed on CRAN checks. Set allow_lambert_s = TRUE in order to 
#'   test this transformation as well. Note that the Lambert of type "h" can also
#'   be done by setting allow_lambert_h = TRUE, however this can take 
#'   significantly longer to run.
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
#' @seealso  \code{\link[bestNormalize]{boxcox}}, \code{\link{orderNorm}},
#'   \code{\link{yeojohnson}}
#' @export
bestNormalize <- function(x, standardize = TRUE, allow_orderNorm = TRUE,
                          allow_lambert_s = FALSE,
                          allow_lambert_h = FALSE,
                          out_of_sample = TRUE, cluster = NULL, k = 10, 
                          warn = TRUE, r = 5) {
  stopifnot(is.numeric(x))
  x.t <- list()
  methods <- c("no_transform", "arcsinh_x", 'boxcox', "exp_x", 
               "log_x", "sqrt_x", 'yeojohnson')
  if (allow_orderNorm) 
    methods <- c(methods, 'orderNorm')
  if(allow_lambert_s)
    methods <- c(methods, "lambert_s")
  if(allow_lambert_h)
    methods <- c(methods, "lambert_h")
  
  
  methods <- methods[order(methods)]
  args <- lapply(methods, function(i) {
    val <- list(standardize = standardize)
    if(i %in% c("orderNorm", "exp_x", "log_x")) {
      val[['warn']] <- warn
    } else if(i == "lambert_s") {
      val[["type"]] <- "s"
    } else if(i == "lambert_h") {
      val[["type"]] <- "h"
    }
    val
  })
  
  method_names <- methods
  method_calls <- gsub("_s|_h", "", methods)

  for(i in 1:length(methods)) {
    args_i <- args[[i]]
    args_i$x <- x
  
     trans_i <- try(do.call(method_calls[i], args_i), silent = TRUE)
    if(is.character(trans_i)) {
      if(warn) 
        warning(paste(method_names[i], ' did not work; ', trans_i))
    } else x.t[[method_names[i]]] <- trans_i
  }

  # Select methods that worked
  method_names <- names(x.t)
  method_calls <- gsub("_s|_h", "", method_names)
  
  ## Estimate out-of-sample P/df
  if(out_of_sample) {
    k <- as.integer(k)
    r <- as.integer(r)
    reps <- get_oos_estimates(x, standardize, method_names, k, r, cluster)
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
    other_transforms = x.t[names(x.t) != best_idx],
    standardize = standardize
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
    ifelse("no_transform" %in% names(x$norm_stats), 
           paste(" - No transform:", round(x$norm_stats['no_transform'], 4), '\n'), ''),
    ifelse("boxcox" %in% names(x$norm_stats), 
           paste(" - Box-Cox:", round(x$norm_stats['boxcox'], 4), '\n'), ''),
    ifelse("lambert_s" %in% names(x$norm_stats), 
           paste(" - Lambert's W (type s):", round(x$norm_stats['lambert_s'], 4), '\n'), ''),
    ifelse("lambert_h" %in% names(x$norm_stats), 
           paste(" - Lambert's W (type h):", round(x$norm_stats['lambert_h'], 4), '\n'), ''),
    ifelse("log_x" %in% names(x$norm_stats), 
           paste(" - Log_b(x+a):", round(x$norm_stats['log_x'], 4), '\n'), ''),
    ifelse("sqrt_x" %in% names(x$norm_stats), 
           paste(" - sqrt(x+a):", round(x$norm_stats['sqrt_x'], 4), '\n'), ''),
    ifelse("exp_x" %in% names(x$norm_stats), 
           paste(" - exp(x):", round(x$norm_stats['exp_x'], 4), '\n'), ''),
    ifelse("arcsinh_x" %in% names(x$norm_stats), 
           paste(" - arcsinh(x):", round(x$norm_stats['arcsinh_x'], 4), '\n'), ''),
    ifelse("yeojohnson" %in% names(x$norm_stats), 
           paste(" - Yeo-Johnson:", round(x$norm_stats['yeojohnson'], 4), '\n'), ''),
    ifelse("orderNorm" %in% names(x$norm_stats), 
           paste(" - orderNorm:", round(x$norm_stats['orderNorm'], 4), '\n'), ''), 
    "Estimation method: ", x$method, '\n')
  
  cat('Best Normalizing transformation with', x$chosen_transform$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(x$chosen_transform)
}

get_oos_estimates <- function(x, standardize, norm_methods, k, r, cluster) {
  x <- x[!is.na(x)]
  fold_size <- floor(length(x) / k)
  if(fold_size < 20) warning("fold_size is ", fold_size, " (< 20), therefore P/df estimates may be off") 
  method_names <- norm_methods
  method_calls <- gsub("_s|_h", "", method_names)
  
  # Perform in this session if cluster unspecified
  if(is.null(cluster)) {
    reps <- lapply(1:r, function(rep) {
      resamples <- create_folds(x, k)
      pstats <- matrix(NA, ncol = length(norm_methods), nrow = k)
      for(i in 1:k) {
        for(m in 1:length(norm_methods)) {
          args <- list(x = x[resamples != i])
          if(method_names[m] == "lambert_h") 
            args$type <- "h"
          trans_m <- suppressWarnings(try(do.call(method_calls[m], args), silent = TRUE))
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
    # Check cluster args
    if (!("cluster" %in% class(cluster))) 
      stop("cluster is not of class 'cluster'; see ?makeCluster")
    
    # Add fns to library
    parallel::clusterCall(cluster, function() library(bestNormalize))
    parallel::clusterExport(cl = cluster, c("k", "x", "norm_methods"), envir = environment())
    reps <- parallel::parLapplyLB(cl = cluster, 1:r, function(rep) {
      method_names <- norm_methods
      method_calls <- gsub("_s|_h", "", method_names)
      
      resamples <- create_folds(x, k)
      pstats <- matrix(NA, ncol = length(norm_methods), nrow = k)
      for(i in 1:k) {
        for(m in 1:length(norm_methods)) {
          args <- list(x = x[resamples != i])
          if(method_names[m] == "lambert_h") 
            args$type <- "h"
          trans_m <-  suppressWarnings(try(do.call(method_calls[m], args), silent = TRUE))
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
