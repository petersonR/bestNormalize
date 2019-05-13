#' Calculate and perform Ordered Quantile normalizing transformation
#'
#' @name orderNorm
#' @aliases predict.orderNorm
#'
#' @description The Ordered Quantile (ORQ) normalization transformation,
#'   \code{orderNorm()}, is a rank-based procedure by which the values of a
#'   vector are mapped to their percentile, which is then mapped to the same
#'   percentile of the normal distribution. Without the presence of ties, this
#'   essentially guarantees that the transformation leads to a uniform
#'   distribution.
#'
#'   The transformation is: \deqn{g(x) = \Phi ^ {-1} ((rank(x) - .5) /
#'   (length(x)))}
#'
#'   Where \eqn{\Phi} refers to the standard normal cdf, rank(x) refers to each
#'   observation's rank, and length(x) refers to the number of observations.
#'
#'   By itself, this method is certainly not new; the earliest mention of it
#'   that I could find is in a 1947 paper by Bartlett (see references). This
#'   formula was outlined explicitly in Van der Waerden, and expounded upon in
#'   Beasley (2009). However there is a key difference to this version of it, as
#'   explained below.
#'
#'   Using linear interpolation between these percentiles, the ORQ normalization
#'   becomes a 1-1 transformation that can be applied to new data. However,
#'   outside of the observed domain of x, it is unclear how to extrapolate the
#'   transformation. In the ORQ normalization procedure, a binomial glm with a
#'   logit link is used on the ranks in order to extrapolate beyond the bounds
#'   of the original domain of x. The inverse normal CDF is then applied to
#'   these extrapolated predictions in order to extrapolate the transformation.
#'   This mitigates the influence of heavy-tailed distributions while preserving
#'   the 1-1 nature of the transformation. The extrapolation will provide a
#'   warning unless warn = FALSE.) However, we found that the extrapolation was
#'   able to perform very well even on data as heavy-tailed as a Cauchy
#'   distribution (paper to be published).
#'
#'   This transformation can be performed on new data and inverted via the
#'   \code{predict} function.
#'
#' @param x A vector to normalize
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'orderNorm'
#' @param warn transforms outside observed range or ties will yield warning
#' @param ... additional arguments
#'
#' @return A list of class \code{orderNorm} with elements
#'
#'   \item{x.t}{transformed original data} \item{x}{original data}
#'   \item{n}{number of nonmissing observations} \item{ties_status}{indicator if
#'   ties are present} \item{fit}{fit to be used for extrapolation, if needed}
#'   \item{norm_stat}{Pearson's P / degrees of freedom}
#'
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'
#' @examples
#'
#' x <- rgamma(100, 1, 1)
#'
#' orderNorm_obj <- orderNorm(x)
#' orderNorm_obj
#' p <- predict(orderNorm_obj)
#' x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
#'
#' all.equal(x2, x)
#' @references
#'
#' Bartlett, M. S. "The Use of Transformations." Biometrics, vol. 3, no. 1,
#' 1947, pp. 39-52. JSTOR www.jstor.org/stable/3001536.
#'
#' Van der Waerden BL. Order tests for the two-sample problem and their power.
#' 1952;55:453-458. Ser A.
#'
#' Beasley TM, Erickson S, Allison DB. Rank-based inverse normal transformations
#' are increasingly used, but are they merited? Behav. Genet. 2009;39(5):
#' 580-595. pmid:19526352
#'
#'
#' @seealso  \code{\link{boxcox}}, \code{\link{lambert}},
#'   \code{\link{bestNormalize}}, \code{\link{yeojohnson}}
#' @importFrom stats qnorm glm
#' @export
orderNorm <- function(x, ..., warn = TRUE) {
  stopifnot(is.numeric(x))
  ties_status <- 0
  nunique <- length(unique(x))
  na_idx <- is.na(x)
  
  if (nunique < length(x)) {
    if(warn) warning('Ties in data, Normal distribution not guaranteed\n')
    ties_status <- 1
  }
  
  q.x <- (rank(x, na.last = 'keep') - .5) / (length(x) - sum(na_idx))
  x.t <- qnorm(q.x)
  
  # fit model for future extrapolation
  fit <- suppressWarnings(
    glm(q.x ~ x, family = 'binomial', 
        weights = rep(length(x) - sum(is.na(x)), length(x)))
  )
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    n = length(x) - sum(is.na(x)),
    ties_status = ties_status,
    fit = fit,
    norm_stat =  unname(ptest$statistic / ptest$df)
  )
  
  class(val) <- 'orderNorm'
  val
}

#' @rdname orderNorm
#' @method predict orderNorm
#' @export
predict.orderNorm <- function(object,
                              newdata = NULL,
                              inverse = FALSE, 
                              warn = TRUE,
                              ...) {
  stopifnot(is.null(newdata) || is.numeric(newdata))
  
  # Perform transformation
  if(!inverse) {
    if(is.null(newdata)) newdata <- object$x
    na_idx <- is.na(newdata)
    
    newdata[!na_idx] <- orderNorm_trans(object, newdata[!na_idx], warn)
    return(newdata)
  } 
  
  # Perform reverse transformation
  if (is.null(newdata)) newdata <- object$x.t
  
  na_idx <- is.na(newdata)
  newdata[!na_idx] <- inv_orderNorm_trans(object, newdata[!na_idx], warn)
  
  return(newdata)
}

#' @rdname orderNorm
#' @method print orderNorm
#' @importFrom stats quantile
#' @export
print.orderNorm <- function(x, ...) {
  cat('orderNorm Transformation with', x$n, 
      'nonmissing obs and', 
      ifelse(
        x$ties_status == 1, 
        paste0('ties\n - ', length(unique(x$x)), ' unique values'),
        'no ties'), '\n',
      '- Original quantiles:\n')
  print(round(quantile(x$x, na.rm = T), 3))
}

#' @importFrom stats approx fitted predict.glm qnorm
orderNorm_trans <- function(orderNorm_obj, new_points, warn) {
  x_t <- orderNorm_obj$x.t
  old_points <- orderNorm_obj$x
  vals <- suppressWarnings(
    approx(old_points, x_t, xout = new_points, rule = 1)
  )
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    if (warn) warning('Transformations requested outside observed domain; logit approx. on ranks applied')
    fit <- orderNorm_obj$fit
    p <- qnorm(fitted(fit, type = "response"))
    l_idx <- vals$x < min(old_points, na.rm = T)
    h_idx <- vals$x > max(old_points, na.rm = T)
    
    # Check 
    if (any(l_idx)) {
      xx <- data.frame(x = vals$x[l_idx])
      vals$y[l_idx] <- qnorm(predict(fit, newdata = xx, type = 'response')) - 
        (min(p, na.rm = T) - min(x_t, na.rm = T))
      
    }
    if (any(h_idx)) {
      xx <- data.frame(x = vals$x[h_idx])
      vals$y[h_idx] <- qnorm(predict(fit, newdata = xx, type = 'response')) - 
        (max(p, na.rm = T) - max(x_t, na.rm = T))
    }
  }
  
  vals$y
}

#' @importFrom stats approx fitted qnorm pnorm
inv_orderNorm_trans <- function(orderNorm_obj, new_points_x_t, warn) {
  x_t <- orderNorm_obj$x.t
  old_points <- orderNorm_obj$x
  vals <- suppressWarnings(
    approx(x_t, old_points, xout = new_points_x_t, rule = 1)
  )
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    if(warn) warning('Transformations requested outside observed domain; logit approx. on ranks applied')
    
    fit <- orderNorm_obj$fit
    p <- qnorm(fitted(fit, type = "response"))
    l_idx <- vals$x < min(x_t, na.rm = T)
    h_idx <- vals$x > max(x_t, na.rm = T)
    
    # Check 
    if (any(l_idx)) {
      # Solve algebraically from original transformation
      logits <- log(pnorm(vals$x[l_idx] + min(p, na.rm = T) - min(x_t, na.rm = T)) / 
                      (1 - pnorm(vals$x[l_idx] + min(p, na.rm = T) - min(x_t, na.rm = T))))
      vals$y[l_idx] <- 
        unname((logits - fit$coef[1]) / fit$coef[2])
    }
    if (any(h_idx)) {
      logits <- log(pnorm(vals$x[h_idx] + max(p, na.rm = T) - max(x_t, na.rm = T)) / 
                      (1 - pnorm(vals$x[h_idx] + max(p, na.rm = T) - max(x_t, na.rm = T))))
      vals$y[h_idx] <- 
        unname((logits - fit$coef[1]) / fit$coef[2])
    }
  }
  
  vals$y
}
