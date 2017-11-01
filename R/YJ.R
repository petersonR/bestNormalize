YJ <- function(x) {
  p <- caret::preProcess(data.frame(x),
                           method = c('YeoJohnson', 'center', 'scale'))
  val <- list(
    x.t = unname(predict(p, newdata = data.frame(x))[, 1]),
    x = x,
    mean = p$mean,
    sd = p$std,
    lambda = unname(p$yj),
    n = length(x.t)
  )
  class(val) <- 'yj'
  val
}

predict.yj <- function(YJ.obj,
                       newdata = NULL,
                       inverse = F) {
  if (is.null(newdata) & !inverse)
    newdata <- YJ.obj$x
  if (is.null(newdata))
    newdata <- YJ.obj$x.t
  
  if (inverse) {
    newdata = newdata * YJ.obj$sd + YJ.obj$mean
    newdata = VGAM::yeo.johnson(newdata, YJ.obj$lambda, inverse = T)
  } else {
    newdata = VGAM::yeo.johnson(newdata, YJ.obj$lambda)
    newdata = (newdata - YJ.obj$mean) / YJ.obj$sd
  }
  
  unname(newdata)
}

print.yj <- function(YJ.obj) {
  cat('Yeo-Johnson Transformation with', YJ.obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      '- lambda =', YJ.obj$lambda, '\n',
      '- mean =', YJ.obj$mean, '\n',
      '- sd =', YJ.obj$sd, '\n')
}
