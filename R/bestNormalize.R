bestNormalize <- function(x,
                          D_max = 1,
                          allow_orderNorm = T,
                          ...) {
  
  x.t <- list()
  x.t[['lw']] <- LW(x)
  x.t[['yj']] <- YJ(x)
  
  if(all(x > 0)) x.t[['bc']] <- BC(x)
  
  D <- unlist(lapply(x.t, function(x)
    unname(nortest::lillie.test(as.vector(x$x.t))$statistic)))

  if (min(D, na.rm = T) > D_max) {
    if (allow_orderNorm) {
      x.t$orderNorm <- orderNorm(x, ...)
      D <- c(D, nortest::lillie.test(as.vector(x.t$orderNorm$x.t))$statistic)
      names(D)[length(D)] <- 'orderNorm'
    } else {
      x.t$binarize <- binarize(x, ...)
      D <- c(D, -Inf)
      names(D)[length(D)] <- 'binarize'
      warning('Data binarized, consider increasing D_max or allowing orderNorm')
    }
  }
  
  xFinal <- x.t[[which.min(D)]]$x.t
  norm_method <- names(x.t)[which.min(D)]
  list(
    method = norm_method,
    x.t = xFinal,
    x = x,
    D = D,
    method_info = x.t[[which.min(D)]]
  )
}

predict.bestNormalize <- function(BNobject,
                      newdata = NULL,
                      inverse = F) {
  method = BNobject$method
  
  # Caret preprocess YJ object's method == 4
  if (length(method) == 4)
    method = 'yj'
  
  obj <- BNobject$method_info
  if (is.null(obj))
    obj <- BNobject
  
  if (method == 'yj') {
    newdata = predict.yj(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'lw') {
    newdata = predict.lw(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'bc') {
    newdata = predict.bc(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'orderNorm') {
    newdata = predict.orderNorm(obj, 
                                newdata = newdata, 
                                inverse = inverse)
  } else if (method == 'binarize') {
    newdata = predict.binarize(obj,
                               newdata = newdata,
                               inverse = inverse)
  } else
    stop('method not recognized')
  
  # Add in more functions here for other methods
  
  newdata
}

