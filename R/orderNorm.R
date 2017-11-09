#' OrderNorm Transformation
#' 
#' @name orderNorm
#' @aliases predict.orderNorm
#'   
#' @description The \code{orderNorm} transformation is a rank-based procedure by
#'   which the values of a vector are mapped to their percentile, which is then
#'   mapped to the same percentile of the normal distribution. Without the
#'   presence of ties, this essentially guarantees that the transformation leads
#'   to a uniform distribution.
#'   
#'   The transformation is: \deqn{g(x) = \Phi ^ {-1} (rank(x) / (length(x) +
#'   1))}
#'   
#'   Where \eqn{\Phi} refers to the standard normal cdf, rank(x) refers to each
#'   observation's rank, and length(x) refers to the number of observations.
#'   
#'   Using linear interpolation between these percentiles, the orderNorm becomes
#'   a 1-1 transformation. While it is possible to extrapolate beyond the
#'   observed values, a warning will occur. This is because outside of the
#'   observed domain of x, it is unclear how to extrapolate the transformation. 
#'   A linear approximation lm(g(x) ~ x) is used to extrapolate beyond the
#'   original domain.
#'   
#'   This transformation can be performed on new data and inverted via the
#'   \code{predict} function.
#'   
#' @param x A vector to normalize
#' @param newdata a vector of data to be (reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param object an object of class 'orderNorm'
#' @param ... additional arguments
#' 
#' @return A list of class \code{orderNorm} with elements
#'   
#' \item{x.t}{transformed original data} 
#' \item{x}{original data} 
#' \item{n}{number of nonmissing observations}
#' \item{warn_status}{indicator if ties are present}
#' \item{fit}{fit to be used for extrapolation, if needed}
#' \item{norm_stat}{Pearson's P / degrees of freedom}
#' The \code{predict} function returns the numeric value of the transformation 
#' performed on new data, and allows for the inverse transformation as well.
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
#' 
#' @seealso  \code{\link{boxcox}},
#'  \code{\link{lambert}}, 
#'  \code{\link{bestNormalize}},
#'  \code{\link{yeojohnson}} 
#' @importFrom stats qnorm lm
#' @export
orderNorm <- function(x) {
  stopifnot(is.numeric(x))
  warn_status <- 0
  nunique <- length(unique(x))
  na_idx <- is.na(x)
  
  if (nunique < length(x)) {
    warning('Ties in data, Normal distribution not guaranteed\n')
    warn_status <- 1
  }
  
  x.t <- qnorm(rank(x, na.last = 'keep') / (length(x) + 1 - sum(na_idx)))
  
  # fit linear model for potential future extrapolation
  fit <- lm(x.t ~ x)
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    n = length(x) - sum(is.na(x)),
    warn_status = warn_status,
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
                              ...) {
  stopifnot(is.null(newdata) || is.numeric(newdata))
  
  # Perform transformation
  if(!inverse) {
    if(is.null(newdata)) newdata <- object$x
    na_idx <- is.na(newdata)
    
    newdata[!na_idx] <- orderNorm_trans(object, newdata[!na_idx])
    return(newdata)
  } 
  
  # Perform reverse transformation
  if (is.null(newdata)) newdata <- object$x.t
  
  na_idx <- is.na(newdata)
  newdata[!na_idx] <- inv_orderNorm_trans(object, newdata[!na_idx])
  
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
        x$warn_status == 1, 
        paste0('ties\n - ', length(unique(x$x)), ' unique values'),
        'no ties'), '\n',
      '- Original quantiles:\n')
  print(round(quantile(x$x), 3))
}

#' @importFrom stats approx fitted predict.lm
orderNorm_trans <- function(orderNorm_obj, new_points) {
  x_t <- orderNorm_obj$x.t
  old_points <- orderNorm_obj$x
  vals <- approx(old_points, x_t, xout = new_points, rule = 1)
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    warning('Transformations requested outside observed domain; linear approx. on ranks applied')
    fit <- orderNorm_obj$fit
    p <- fitted(fit)
    l_idx <- vals$x < min(old_points)
    h_idx <- vals$x > max(old_points)
    
    # Check 
    if (any(l_idx)) {
      xx <- data.frame(x = vals$x[l_idx])
      vals$y[l_idx] <- predict(fit, newdata = xx) - (min(p) - min(x_t))
      
    }
    if (any(h_idx)) {
      xx <- data.frame(x = vals$x[h_idx])
      vals$y[h_idx] <- predict(fit, newdata = xx) - (max(p) - max(x_t))
    }
  }
  
  vals$y
}

#' @importFrom stats approx fitted
inv_orderNorm_trans <- function(orderNorm_obj, new_points_x_t) {
  x_t <- orderNorm_obj$x.t
  old_points <- orderNorm_obj$x
  vals <- approx(x_t, old_points, xout = new_points_x_t, rule = 1)
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    warning('Transformations requested outside observed domain; linear approx. on ranks applied')
    
    fit <- orderNorm_obj$fit
    p <- fitted(fit)
    l_idx <- vals$x < min(x_t)
    h_idx <- vals$x > max(x_t)
    
    # Check 
    if (any(l_idx)) {
      # Solve algebraically from original transformation
      vals$y[l_idx] <- 
        unname((vals$x[l_idx] + min(p) - min(x_t) - fit$coef[1]) / fit$coef[2])
    }
    if (any(h_idx)) {
      vals$y[h_idx] <- 
        unname((vals$x[h_idx] + max(p) - max(x_t) - fit$coef[1]) / fit$coef[2])
    }
  }
  
  vals$y
}
