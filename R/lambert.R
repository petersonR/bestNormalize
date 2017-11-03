
# NOTE: The type = 's' argument is the only one that does the 1-1 transform consistently
# Use type = 'h' or type = 'hh' at risk of not having this 1-1 transform
lambert <- function(x, type = 's', ...) {
  obj <- unname(LambertW::Gaussianize(x, type = type, return.tau.mat = T, ...))
  x.t <- obj[[1]]
  tau.mat <- obj[[2]]
  
  mu <- mean(x.t)
  sigma <- sd(x.t)
  
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
  
  class(val) <- 'lambert'
  val
}

predict.lambert <- function(lambert_obj,
                            newdata = NULL,
                            inverse = F) {
  if (is.null(newdata) & !inverse)
    newdata <- lambert_obj$x
  if (is.null(newdata) & inverse)
    newdata <- lambert_obj$x.t
  
  if (inverse)
    newdata <- newdata * lambert_obj$sd + lambert_obj$mean
  
  newdata <- LambertW::Gaussianize(
    as.matrix(newdata),
    type = lambert_obj$type,
    tau.mat = lambert_obj$tau.mat,
    inverse = inverse
  )
  
  if (!inverse)
    newdata <- (newdata - lambert_obj$mean) / lambert_obj$sd
  attributes(newdata) <- NULL
  
  unname(newdata)
}

print.lambert <- function(lambert_obj) {
  prettyTau <- apply(cbind('- ',
    rownames(lambert_obj$tau.mat)[-c(1:2)], ' = ',
    round(lambert_obj$tau.mat[-c(1:2)],4), '\n'
  ), 1, paste, collapse = '')

  
  cat('Lambert WxF Transformation of type', lambert_obj$type, 
      'with', lambert_obj$n, 'observations:\n', 
      'Estimated statistics:\n',
      prettyTau,
      '- mean =', lambert_obj$mean, '\n',
      '- sd =', lambert_obj$sd, '\n')
}
