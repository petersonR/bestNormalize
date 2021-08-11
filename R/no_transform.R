#' Identity transformation and center/scale transform
#'
#' @name no_transform
#' @aliases predict.no_transform center_scale 
#'
#' @description Perform an identity transformation. Admittedly it seems odd to
#'   have a dedicated function to essentially do I(x), but it makes sense to
#'   keep the same syntax as the other transformations so it plays nicely
#'   with them. As a benefit, the bestNormalize function will also show
#'   a comparable normalization statistic for the untransformed data. If 
#'   \code{standardize == TRUE}, \code{center_scale} passes to bestNormalize instead.
#' @param x A vector 
#' @param warn Should a warning result from infinite values?
#' @param object an object of class 'no_transform'
#' @param newdata a vector of data to be (potentially reverse) transformed
#' @param inverse if TRUE, performs reverse transformation
#' @param ... additional arguments
#' @details \code{no_transform} creates a identity transformation object 
#'   that can be applied to new data via the \code{predict} function.
#'
#' @return A list of class \code{no_transform} with elements
#'   \item{x.t}{transformed original data} 
#'   \item{x}{original data}
#'   \item{n}{number of nonmissing observations}
#'   \item{norm_stat}{Pearson's P / degrees of freedom} 
#'
#'   The \code{predict} function returns the numeric value of the transformation
#'   performed on new data, and allows for the inverse transformation as well.
#'   
#' @examples
#' x <- rgamma(100, 1, 1)
#'
#' no_transform_obj <- no_transform(x)
#' no_transform_obj
#' p <- predict(no_transform_obj)
#' x2 <- predict(no_transform_obj, newdata = p, inverse = TRUE)
#'
#' all.equal(x2, x)
#'
#' @importFrom stats sd
#' @export
no_transform <- function(x, warn = TRUE, ...) {
  stopifnot(is.numeric(x))
  
  x.t <- x
  
  if (all(infinite_idx <- is.infinite(x.t))) {
    stop("Transformation infinite for all x")
  }
  if(any(infinite_idx) & warn) {
    warning("Some values (but not all) transformed values are infinite")
  }
  
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)

  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df)
  )
  class(val) <- c('no_transform', class(val))
  val
}

#' @rdname no_transform
#' @method predict no_transform
#' @export
predict.no_transform <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    newdata <-  newdata
  } else if (!inverse) {
    newdata <- newdata
  }
  unname(newdata)
}

#' @rdname no_transform
#' @method print no_transform
#' @export
print.no_transform <- function(x, ...) {
  cat('I(x) Transformation with', x$n, 'nonmissing obs.\n')
}

#' @rdname no_transform
#' @importFrom stats sd
#' @export
center_scale <- function(x, warn = TRUE, ...) {
  stopifnot(is.numeric(x))
  
  x.t <- x
  
  if (all(infinite_idx <- is.infinite(x.t))) {
    stop("Transformation infinite for all x")
  }
  if(any(infinite_idx) & warn) {
    warning("Some values (but not all) transformed values are infinite")
  }
  
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  
  x.t <- (x.t - mu) / sigma
  
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    mean = mu,
    sd = sigma,
    n = length(x.t) - sum(is.na(x)),
    norm_stat = unname(ptest$statistic / ptest$df)
  )
  class(val) <- c('center_scale', class(val))
  val
}

#' @rdname no_transform
#' @method predict center_scale
#' @export
predict.center_scale <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  if (inverse) {
    newdata <- newdata * object$sd + object$mean
  } else if (!inverse) {
    newdata <- (newdata - object$mean) / object$sd
  }
  unname(newdata)
}

#' @rdname no_transform
#' @method print center_scale
#' @export
print.center_scale <- function(x, ...) {
  cat('center_scale(x) Transformation with', x$n, 'nonmissing obs.\n', 
      'Estimated statistics:\n',
      '- mean (before standardization) =', x$mean, '\n',
      '- sd (before standardization) =', x$sd, '\n')
}

#' @rdname no_transform
#' @param x A `no_transform` object.
#' @importFrom tibble tibble
#' @export
tidy.no_transform <- function(x) {
  value <- tibble(
    "transform" = c("no_transform"),
    "norm_stat" = x$norm_stat, 
    "chosen" = 1
  )
}
