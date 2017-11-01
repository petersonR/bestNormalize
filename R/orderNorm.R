orderNorm <- function(x) {
  warn_status <- 0
  nunique <- length(unique(x))
  
  if (nunique < length(x)) {
    warning('Ties in data, Normal distribution not guaranteed')
    warn_status <- 1
  }
  
  raw_x.t <- qnorm(rank(x, na.last = 'keep') / (length(x) + 1))
  
  # fit model for interpolation and extrapolation
  s_fit <- gam(raw_x.t ~ s(x, k = nunique))
  
  # fit model for reverse transformation
  rev_s_fit <- gam(x ~ s(raw_x.t, k = nunique))
  
  x.t <- unname(as.vector(predict(s_fit)))
  list(
    x.t = x.t,
    x = x,
    n = length(x),
    method = 'orderNorm',
    warn_status = warn_status,
    s_fit = s_fit,
    rev_s_fit = rev_s_fit
  )
}

predict.orderNorm <- function(orderNorm.obj,
                              newdata = NULL,
                              inverse = FALSE) {
  
  if (is.null(newdata) & !inverse)
    newdata <- orderNorm.obj$x
  if (is.null(newdata))
    newdata <- orderNorm.obj$x.t
    
  fitName <- ifelse(inverse, 'rev_s_fit', 's_fit')
  
  newdata <- data.frame(newdata)
  names(newdata) <- ifelse(inverse, 'raw_x.t', 'x')
  
  vals <-
    mgcv::predict.gam(orderNorm.obj[[fitName]], newdata = newdata)
  unname(as.vector(vals))
}
