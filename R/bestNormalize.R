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
#' @param allow_lambert_s Set to FALSE if the lambertW of type "s"  should not be
#'   applied (see details). Expect about 2-3x elapsed computing time if TRUE.
#' @param allow_lambert_h Set to TRUE if the lambertW of type "h"  should be
#'   applied (see details). Expect about 2-3x elapsed computing time. 
#' @param allow_exp Set to TRUE if the exponential transformation should be
#'   applied (sometimes this will cause errors with heavy right skew)
#' @param standardize If TRUE, the transformed values are also centered and
#'   scaled, such that the transformation attempts a standard normal. This will
#'   not change the normality statistic.
#' @param out_of_sample if FALSE, estimates quickly in-sample performance
#' @param cluster name of cluster set using \code{makeCluster}
#' @param k number of folds
#' @param r number of repeats
#' @param loo should leave-one-out CV be used instead of repeated CV? (see
#'   details)
#' @param warn Should bestNormalize warn when a method doesn't work?
#' @param quiet Should a progress-bar not be displayed for cross-validation
#'   progress?
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'bestNormalize'
#' @param tr_opts a list (of lists), specifying options to be passed to each
#'   transformation (see details)
#' @param new_transforms a named list of new transformation functions and their
#'   predict methods (see details)
#' @param norm_stat_fn if specified, a function to calculate to assess normality
#'   (default is the pearson chi-squared statistic divided by its d.f.)
#' @param ... additional arguments.
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
#'   \item{oos_preds}{Out-of-sample predictions (if loo == TRUE) or
#'   normalization stats}
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
#'
#'   Repeated cross-validation is used by default to estimate the out-of-sample
#'   performance of each transformation if out_of_sample = TRUE. While this can
#'   take some time, users can speed it up by creating a cluster via the
#'   \code{parallel} package's \code{makeCluster} function, and passing the name
#'   of this cluster to \code{bestNormalize} via the cl argument. For best
#'   performance, we recommend the number of clusters to be set to the number of
#'   repeats r. Care should be taken to account for the number of observations
#'   per fold; too small a number and the estimated normality statistic could be
#'   inaccurate, or at least suffer from high variability.
#'
#'
#'   As of version 1.3, users can use leave-one-out cross-validation as well for
#'   each method by setting \code{loo} to \code{TRUE}.  This will take a lot of
#'   time for bigger vectors, but it will have the most accurate estimate of
#'   normalization efficacy. Note that if this method is selected, arguments
#'   \code{k, r} are ignored. This method will still work in parallel with the
#'   \code{cl} argument.
#'
#'
#'   Note that the Lambert transformation of type "h" can be done by setting
#'   allow_lambert_h = TRUE, however this can take significantly longer to run.
#'
#'   Use \code{tr_opts} in order to set options for each transformation. For
#'   instance, if you want to overide the default a selection for \code{log_x},
#'   set \code{tr_opts$log_x = list(a = 1)}.
#'
#'   See the package's vignette on how to use custom functions with
#'   bestNormalize. All it takes is to create an S3 class and predict method for
#'   the new transformation and load it into the environment, then the new
#'   custom function (and its predict method) can be passed to bestNormalize
#'   with \code{new_transform}.
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
#'
#' \dontrun{
#' # With leave-one-out CV
#' BN_obj <- bestNormalize(x, loo = TRUE)
#' BN_obj
#' p <- predict(BN_obj)
#' x2 <- predict(BN_obj, newdata = p, inverse = TRUE)
#'
#' all.equal(x2, x)
#' }
#'
#' # Without CV
#' BN_obj <- bestNormalize(x, allow_orderNorm = FALSE, out_of_sample = FALSE)
#' BN_obj
#' p <- predict(BN_obj)
#' x2 <- predict(BN_obj, newdata = p, inverse = TRUE)
#'
#' all.equal(x2, x)
#'
#' @seealso  \code{\link[bestNormalize]{boxcox}}, \code{\link{orderNorm}},
#'   \code{\link{yeojohnson}}
#' @export
bestNormalize <- function(x, standardize = TRUE, 
                          allow_orderNorm = TRUE,
                          allow_lambert_s = FALSE,
                          allow_lambert_h = FALSE,
                          allow_exp = TRUE,
                          out_of_sample = TRUE, 
                          cluster = NULL, 
                          k = 10, 
                          r = 5, 
                          loo = FALSE,
                          warn = FALSE, 
                          quiet = FALSE, 
                          tr_opts = list(),
                          new_transforms = list(), 
                          norm_stat_fn= NULL, ...) {
  stopifnot(is.numeric(x))
  x.t <- list()
  methods <- c("no_transform", "arcsinh_x", 'boxcox', 
               "log_x", "sqrt_x", 'yeojohnson')
  
  # Change no transform to center_scale if standardize = T
  if(standardize)
    methods <- gsub("no_transform", "center_scale", methods)
  
  if(allow_exp) 
    methods <- c(methods, "exp_x")
  if (allow_orderNorm) 
    methods <- c(methods, 'orderNorm')
  if(allow_lambert_s)
    methods <- c(methods, "lambert_s")
  if(allow_lambert_h)
    methods <- c(methods, "lambert_h")
  
  # Check for new methods
  if(length(new_transforms)) {
    stopifnot(all(sapply(new_transforms, is.function)))
    
    # Makes sure predict methods are found
    found_predict_methods <- names(new_transforms)[!grepl('predict|print', names(new_transforms))] %in%
      gsub("predict\\.", "", names(new_transforms)[grepl('predict', names(new_transforms))])
    
    
    if(any(!found_predict_methods))
      stop("'new_transforms' associated predict methods must be supplied")
    methods <- c(methods, names(new_transforms)[!grepl('predict|print', names(new_transforms))])
    
    # Make sure R can find new method function calls
    for (m in 1:length(new_transforms)) 
      assign(names(new_transforms)[m], new_transforms[[m]])
  }
  
  
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
  method_calls <- gsub("lambert_s|lambert_h", "lambert", methods)
  names(args) <- methods
  
  ## Set transformation options (if any)
  if(length(tr_opts)) {
    stopifnot(is.list(tr_opts))
    if(any(nr <- !(names(tr_opts) %in% names(args))))
      stop(names(tr_opts)[nr], " not recognized")
    
    for(i in 1:length(tr_opts)) {
      tr <- names(tr_opts)[i]
      args[[tr]] <- c(args[[tr]], tr_opts[[tr]])
    }
  }
  

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
  method_calls <- gsub("lambert_s|lambert_h", "lambert", method_names)
  
  # Normality statistic if not specified
  if(!length(norm_stat_fn)) {
    norm_stat_fn <- function(x) {
      val <- nortest::pearson.test(x)
      unname(val$stat/val$df)
    }
    custom_norm_stat_fn <- TRUE
  } else
    custom_norm_stat_fn <- FALSE
    
  
  stopifnot(is.function(norm_stat_fn))
  
  ## Estimate out-of-sample P/df
  if(out_of_sample) {
    ## Repeated CV
    if(!loo) {
      k <- as.integer(k)
      r <- as.integer(r)
      oos_est <- get_oos_estimates(x, standardize, method_names, k, r, 
                                   cluster, quiet, warn, args, new_transforms, 
                                   norm_stat_fn)
      oos_preds <- oos_est$oos_preds
      norm_stats <- oos_est$norm_stats
      best_idx <- names(which.min(norm_stats))
      method <- paste("Out-of-sample via CV with", k, "folds and", r, "repeats")
    } else {
      ## Leave one out CV
      loo_est <- get_loo_estimates(x, standardize, method_names, 
                                   cluster, quiet, warn, args, new_transforms, 
                                   norm_stat_fn)
      oos_preds <- loo_est$oos_preds
      norm_stats <- loo_est$norm_stats
      best_idx <- names(which.min(norm_stats))
      method <- paste("Out-of-sample via leave-one-out CV")
      
    }
    
  } else {
    norm_stats <- unlist(lapply(x.t, function(x) norm_stat_fn(x$x.t)))
    names(norm_stats) <- names(x.t)
    best_idx <- names(which.min(norm_stats))
    method <- "In-sample"
    oos_preds <- NULL
  }
  
  val <- list(
    x.t = x.t[[best_idx]]$x.t,
    x = x,
    norm_stats = norm_stats,
    method = method,
    oos_preds = oos_preds,
    chosen_transform = x.t[[best_idx]],
    other_transforms = x.t[names(x.t) != best_idx],
    standardize = standardize, 
    new_transforms = new_transforms, 
    norm_stat_fn= norm_stat_fn,
    custom_norm_stat_fn = custom_norm_stat_fn
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
  
  prettynames <- c(
    "arcsinh_x" = "arcsinh(x)",
    "boxcox" = "Box-Cox" ,
    "exp_x" = "Exp(x)",
    "lambert_h" = "Lambert's W (type h)",
    "lambert_s" = "Lambert's W (type s)",
    "log_x" = "Log_b(x+a)",
    "no_transform" = "No transform",
    "orderNorm" = "orderNorm (ORQ)",
    "sqrt_x" = "sqrt(x + a)",
    "yeojohnson" = "Yeo-Johnson",
    "center_scale" = "Center+scale"
  )
  
  normnames <- names(x$norm_stats)
  
  for(i in 1:length(prettynames))
    normnames <- gsub(names(prettynames)[i],prettynames[i],normnames)
  
  results <- paste0(" - ", normnames, ": ", round(x$norm_stats, 4), collapse="\n")
  
  prettyD <- paste0(
    'Estimated Normality Statistics ',
    ifelse(x$custom_norm_stat_fn, 
           "(Pearson P / df, lower => more normal):\n", 
           "(using custom normalization statistic)\n"),
    results, 
    "\nEstimation method: ", x$method, '\n')
  
  cat('Best Normalizing transformation with', x$chosen_transform$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(x$chosen_transform)
}

#' @rdname bestNormalize
#' @param x A `bestNormalize` object.
#' @importFrom tibble tibble
#' @export
tidy.bestNormalize <- function(x) {
  
  chosen_name <- class(x$chosen_transform)[1]
  chosen_tr <- x$chosen_transform
  chosen <- list(chosen_tr)
  names(chosen)[1] <- chosen_name
  
  method_names <- names(x$norm_stats)
  other_method_names <- method_names[method_names!= chosen_name]
  
  value <- tibble(
    "transform" = c(chosen_name, other_method_names),
    "tr_object" = c(chosen, x$other_transforms),
    "norm_stat" = x$norm_stats[c(chosen_name, other_method_names)],
    "chosen" = c(1, rep(0, length(other_method_names)))
  )
}


# Get out-of-sample normality statistics via repeated CV
#' @importFrom doParallel registerDoParallel
#' @importFrom doRNG "%dorng%"
#' @importFrom methods is
#' @importFrom foreach foreach
get_oos_estimates <- function(x, standardize, norm_methods, k, r, 
                              cluster, quiet, warn, args, new_transforms, 
                              norm_stat_fn) {
  x <- x[!is.na(x)]
  fold_size <- floor(length(x) / k)
  if(fold_size < 20 & warn) 
    warning("CV fold size is ", fold_size, " (< 20), therefore P/df estimates may be off") 
  if (fold_size < 3) 
    stop("Cannot do k-fold CV with fold size < 3; decrease k, set loo = TRUE, or out_of_sample = FALSE")
  method_names <- norm_methods
  method_calls <- gsub("lambert_s|lambert_h", "lambert", method_names)
  
  # Perform in this session if cluster unspecified
  if(is.null(cluster)) {
    
    if(!quiet & length(x) > 2000) pb <- dplyr::progress_estimated(r*k)
    
    reps <- lapply(1:r, function(rep) {
      resamples <- create_folds(x, k)
      pstats <- matrix(NA, ncol = length(method_names), nrow = k)
      for(i in 1:k) {
        for(m in 1:length(method_names)) {
          args_m <- args[[m]]
          args_m$x <- x[resamples != i]
          trans_m <- suppressWarnings(try(do.call(method_calls[m], args_m), silent = TRUE))
          if(is(trans_m, "try-error")) {
            if(warn) warning(paste(norm_methods[m], ' did not work; ', trans_m))
            pstats[i, m] <- NA
          } else {
            vec <- suppressWarnings(predict(trans_m, newdata = x[resamples == i]))
            pstats[i, m] <- suppressWarnings(do.call(norm_stat_fn, list(x = vec)))
          }
        }
        if(!quiet & length(x) > 2000) pb$tick()$print()
      }
      colnames(pstats) <- norm_methods
      rownames(pstats) <- paste0("Rep", rep, "Fold", 1:k)
      pstats
    })
    reps <- Reduce(rbind, reps)
  } else {
    # Check cluster args
    if (!is(cluster, "cluster")) 
      stop("cluster is not of class 'cluster'; see ?makeCluster")
    
    # Assign new transforms to functional env.
    if(length(new_transforms)) {
      for (m in 1:length(new_transforms)) 
        assign(names(new_transforms)[m], new_transforms[[m]])
    }
    
    # Add fns to library
    parallel::clusterCall(cluster, function() library(bestNormalize))
    parallel::clusterExport(cl = cluster, c("k", "x", "norm_methods", "args", names(new_transforms)), envir = environment())
    doParallel::registerDoParallel(cluster)
    
    opts <- list()
    if(!quiet & length(x) > 2000) {
      pb <- dplyr::progress_estimated(k*r)
      
      progress <- function() pb$tick()$print()
      opts <- list(progress=progress)
    }
    
    # Metadata for cv procedure
    cvparams <- expand.grid(i = 1:k, rep = 1:r)[,2:1]
    resamples <- sapply(1:r, function(ii) create_folds(x=x, k=k))
    `%dorng%` <- doRNG::`%dorng%`
    reps <- foreach::foreach(
      rep = 1:nrow(cvparams),
      .combine = "rbind",
      .options.snow = opts
    ) %dorng% {
     rr <- cvparams$rep[rep]
     i <- cvparams$i[rep]
     resample <- resamples[,rr]
     ip <- numeric(length(norm_methods))
     for(m in 1:length(norm_methods)) {
       args_m <- args[[m]]
       args_m$x <- x[resamples != i]
       trans_m <-  suppressWarnings(try(do.call(method_calls[m], args_m), silent = TRUE))
       if(is(trans_m, "try-error")) {
         if(warn) warning(paste(norm_methods[m], ' did not work; ', trans_m))
         ip[m] <- NA
       } else {
         vec <- suppressWarnings(predict(trans_m, newdata = x[resamples == i]))
         ip[m] <- suppressWarnings(do.call(norm_stat_fn, list(x = vec)))
       }
     }
     ip
    }
  }
  if(!quiet & length(x) > 2000) cat("\n")
  colnames(reps) <- norm_methods
  list(norm_stats = colMeans(reps), oos_preds = as.data.frame(reps))
}

create_folds <- function(x, k) { 
  as.numeric(cut(sample(1:length(x)), breaks = k))
}

# Get out-of-sample normality statistics via leave-one-out CV
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom methods is
get_loo_estimates <- function(x, standardize, norm_methods, cluster, quiet, warn, 
                              args, new_transforms, norm_stat_fn) {
  x <- x[!is.na(x)]
  n <- length(x)
  method_names <- norm_methods
  method_calls <- gsub("lambert_s|lambert_h", "lambert", method_names)
  
  # Perform in this session if cluster unspecified
  if(is.null(cluster)) {
    
    if(!quiet & length(x) > 2000) 
      cat("Note: passing a cluster (?makeCluster) to bestNormalize can speed up CV process\n")
    
    # For every subject and normalization method, create prediction
    p <- matrix(NA, ncol = length(norm_methods), nrow = n)
    if(!quiet & length(x) > 2000) pb <- dplyr::progress_estimated(n)
    for(i in 1:n) {
      for(m in 1:length(norm_methods)) {
        args_m <- args[[m]]
        args_m$x <- x[-i]
        trans_m <- suppressWarnings(try(do.call(method_calls[m], args_m), silent = TRUE))
        if(is(trans_m, "try-error")) {
          stop(paste(norm_methods[m], ' did not work; ', trans_m))
          p[i, m] <- NA
        } else {
          p[i,m] <- suppressWarnings(predict(trans_m, newdata = x[i]))
          
        }
      }
      if(!quiet & length(x) > 2000) pb$tick()$print()
    }
    colnames(p) <- norm_methods
  
  } else {
    # Check cluster args
    if (!is(cluster, "cluster")) 
      stop("cluster is not of class 'cluster'; see ?makeCluster")
    
    # Assign new transforms to functional env.
    if(length(new_transforms)) {
      for (m in 1:length(new_transforms)) 
        assign(names(new_transforms)[m], new_transforms[[m]])
    }
    
    # Add fns to library
    parallel::clusterCall(cluster, function() library(bestNormalize))
    parallel::clusterExport(cl = cluster, c("x", "norm_methods", "args", names(new_transforms)), envir = environment())
    
    doParallel::registerDoParallel(cluster)
    
    opts <- list()
    if(!quiet & length(x) > 2000) {
      pb <- dplyr::progress_estimated(n)
      
      progress <- function() pb$tick()$print()
      opts <- list(progress=progress)
    }
    
    `%dopar%` <- foreach::`%dopar%`
    
    p <- foreach::foreach(
      i = 1:n,
      .combine = "rbind",
      .options.snow = opts) %dopar% {               
      ip <- numeric(length(norm_methods))
      for(m in 1:length(norm_methods)) {
        args_m <- args[[m]]
        args_m$x <- x[-i]
        trans_m <- suppressWarnings(try(do.call(method_calls[m], args_m), silent = TRUE))
        if(is(trans_m, "try-error")) {
          stop(paste(norm_methods[m], ' did not work; ', trans_m))
          ip[m] <- NA
        } else {
          ip[m] <- suppressWarnings(predict(trans_m, newdata = x[i]))
        }
      }
      ip
    }
    colnames(p) <- norm_methods
  }
  normstats <- apply(p, 2, norm_stat_fn)
  if(!quiet & length(x) > 2000) cat("\n")
  list(norm_stats = normstats, oos_preds = as.data.frame(p))
}