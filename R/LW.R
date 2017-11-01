LW <- function(x, type = 's') {
  obj <- unname(LambertW::Gaussianize(x, type = type, return.tau.mat = T))
  x.t <- obj[[1]]
  tau.mat <- obj[[2]]
  
  mu <- mean(x.t)
  sigma <- sd(x.t)
  
  x.t_uncentered <- x.t
  x.t <- (x.t - mu) / sigma
  
  attributes(x.t) <- NULL
  
  val <- list(
    x.t = unname(x.t),
    x = x,
    mean = mu,
    sd = sigma,
    tau.mat = tau.mat,
    n = length(x.t),
    type = type
  )
  
  class(val) <- 'lw'
  val
}

predict.lw <- function(LW.obj,
                       newdata = NULL,
                       inverse = F) {
  if (is.null(newdata) & !inverse)
    newdata <- LW.obj$x
  if (is.null(newdata) & inverse)
    newdata <- LW.obj$x.t
  
  if (inverse)
    newdata = newdata * LW.obj$sd + LW.obj$mean
  
  newdata <- LambertW::Gaussianize(
    as.matrix(newdata),
    type = LW.obj$type,
    tau.mat = LW.obj$tau.mat,
    inverse = inverse
  )
  
  if (!inverse)
    newdata = (newdata - LW.obj$mean) / LW.obj$sd
  attributes(newdata) <- NULL
  
  unname(newdata)
}

print.lw <- function(LW.obj) {
  cat('LambertW::Gaussianize Transformation with', LW.obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      '- gamma =', LW.obj$tau.mat[3], '\n',
      '- mean =', LW.obj$mean, '\n',
      '- sd =', LW.obj$sd, '\n')
}
