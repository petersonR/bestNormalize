orderNorm <- function(x) {
  warn_status <- 0
  nunique <- length(unique(x))
  
  if (nunique < length(x)) {
    warning('Ties in data, Normal distribution not guaranteed')
    warn_status <- 1
  }
  
  x.t <- qnorm(rank(x, na.last = 'keep') / (length(x) + 1))
  
  # fit linear model for potential future extrapolation
  fit <- lm(x.t ~ x)
  
  val <- list(
    x.t = x.t,
    x = x,
    n = length(x),
    method = 'orderNorm',
    warn_status = warn_status,
    fit = fit
  )
  
  class(val) <- 'orderNorm'
  val
}

predict.orderNorm <- function(orderNorm.obj,
                              newdata = NULL,
                              inverse = FALSE) {
  # Perform transformation
  if(!inverse) {
    if(is.null(newdata)) newdata <- orderNorm.obj$x
    return(x2orderNorm(orderNorm.obj, newdata))
  } 
  
  # Perform reverse transformation
  if (is.null(newdata)) newdata <- orderNorm.obj$x.t
  orderNorm2x(orderNorm.obj, newdata)
}

print.orderNorm <- function(orderNorm.obj) {
  cat('Smooth OrderNorm Transformation with', orderNorm.obj$n, 
      'observations and', 
      ifelse(
        orderNorm.obj$warn_status == 1, 
        paste0('ties\n - ', length(unique(orderNorm.obj$x)), ' unique values'),
        'no ties'), '\n')
}

x2orderNorm <- function(orderNorm.obj, new_points) {
  x_t <- orderNorm.obj$x.t
  old_points <- orderNorm.obj$x
  vals <- approx(old_points, x_t, xout = new_points, rule = 1)
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    warning('Transformations requested outside observed domain; linear approx. on ranks applied')
    fit <- orderNorm.obj$fit
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

orderNorm2x <- function(orderNorm.obj, new_points_x_t) {
  x_t <- orderNorm.obj$x.t
  old_points <- orderNorm.obj$x
  vals <- approx(x_t, old_points, xout = new_points_x_t, rule = 1)
  
  # If predictions have been made outside observed domain
  if (any(is.na(vals$y))) {
    warning('Transformations requested outside observed domain; linear approx. on ranks applied')
    
    fit <- orderNorm.obj$fit
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
