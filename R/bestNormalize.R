bestNormalize <- function(x,
                          D_max = 1,
                          allow_orderNorm = T,
                          ...) {
  
  x.t <- list()
  x.t[['lw']] <- Lambert(x)
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
