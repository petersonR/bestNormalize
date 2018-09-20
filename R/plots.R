#' Transformation plotting
#'
#' Plots transformation functions for objects produced by the bestNormalize
#' package
#'
#' @name plot.bestNormalize
#' @aliases plot.orderNorm plot.boxcox plot.yeojohnson plot.lambert
#'
#' @details The plots produced by the individual transformations are simply
#'   plots of the original values by the newly transformed values, with a line
#'   denoting where transformations would take place for new data.
#'
#'   For the bestNormalize object, this plots each of the possible
#'   transformations run by the original call to bestNormalize. The first
#'   argument in the "cols" parameter refers to the color of the chosen
#'   transformation.
#'   
#' @rdname plot.bestNormalize
#' @param x a fitted transformation
#' @param inverse if TRUE, plots the inverse transformation
#' @param bounds a vector of bounds to plot for the transformation
#' @param cols a vector of colors to use for the transforms (see details)
#' @param methods a vector of transformations to plot
#' @param leg_loc the location of the legend on the plot
#' @param ... further parameters to be passed to \code{plot} and \code{lines}
#' @importFrom graphics legend lines plot points
#' @export
plot.bestNormalize <- function(x, inverse = FALSE, bounds = NULL, 
                               cols = c("green3", 1, 2, 4:6), 
                               methods = c('boxcox', 'yeojohnson', "orderNorm", 
                                           "lambert_s", "lambert_h"), 
                               leg_loc = 'top', 
                               ...) {
  
  if(!inverse) {
    xvals <- x$x 
    x_t <- x$x.t
  } else {
    xvals <- x$x.t
    x_t <- x$x
  }
  
  if(is.null(bounds)) {
    xx <- seq(min(xvals), max(xvals), length = 1000)
  } else 
    xx <- seq(min(bounds), max(bounds), length = 1000)
  
  yy <- predict(x, newdata = xx, inverse = inverse, warn = FALSE)
  methods <- methods[methods != names(x$norm_stats)[which.min(x$norm_stats)]]
  methods <- methods[methods %in% names(x$norm_stats)]
  ys <- lapply(methods, function(i) {
    obj <- x$other_transforms[[i]]
    y_i <- predict(obj, newdata = xx, inverse = inverse, warn = FALSE)
    y_i
  })
  
  plot(xx, yy, ylim = range(yy, ys, na.rm = TRUE), 
       xlim = range(xx), type = 'l', 
       col = cols[1], lwd = 2, 
       xlab = ifelse(inverse, "g(x)", "x"), 
       ylab = ifelse(!inverse, "g(x)", "x"),  
       ...)
  
  lapply(1:length(ys), function(i) {lines(xx, ys[[i]], col = cols[i + 1], lwd = 2, ...)})
  
  labs <- c(class(x$chosen_transform), methods)
  
  legend(leg_loc, labs, col = cols, bty = 'n', lwd = 2)
  if(!inverse) 
    points(x = xvals, y = rep(min(range(ys, yy, na.rm = T)), length(xvals)), pch = '|')
  else 
    points(x =  rep(min(xvals, bounds), length(x$x)), y = x$x, pch = '_')
  invisible(x)
}

#' @rdname plot.bestNormalize
#' @importFrom graphics lines plot
#' @export
plot.orderNorm <- function(x, inverse = FALSE, bounds = NULL, ...) {
  
  if(!inverse) {
    xvals <- x$x 
    x_t <- x$x.t
  } else {
    xvals <- x$x.t
    x_t <- x$x
  }
    
  if(is.null(bounds)) {
    xx <- seq(min(xvals), max(xvals), length = 1000)
  } else 
    xx <- seq(min(bounds), max(bounds), length = 100)
    
  yy <- predict(x, newdata = xx, inverse = inverse, warn = FALSE)
  plot(xvals, x_t, pch = 20, ylim = range(yy, na.rm = TRUE), xlim = range(xx), ...)
  lines(xx, yy, col = 'slateblue', lwd = 2, ...)
  invisible(x)
}

#' @rdname plot.bestNormalize
#' @importFrom graphics lines plot
#' @export
plot.boxcox <- plot.orderNorm

#' @rdname plot.bestNormalize
#' @importFrom graphics lines plot
#' @export
plot.yeojohnson <- plot.orderNorm

#' @rdname plot.bestNormalize
#' @importFrom graphics lines plot
#' @export
plot.lambert <- plot.orderNorm

