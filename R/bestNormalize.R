bestNormalize <- function(x,
                          D_max = 1,
                          allow_orderNorm = T,
                          ...) {
  
  x.t <- list()
  x.t[['lambert']] <- lambert(x)
  x.t[['yj']] <- YJ(x)
  
  if (all(x > 0)) x.t[['boxcox']] <- boxcox(x)
  
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
  val <- list(
    x.t = xFinal,
    x = x,
    D = D,
    n = length(xFinal),
    chosen_transform = x.t[[which.min(D)]]
  )
  class(val) <- 'bestNormalize'
  val
}

predict.bestNormalize <- function(BNobject, newdata = NULL, inverse = FALSE) {
  obj <- BNobject$chosen_transform
  predict(obj, newdata = newdata, inverse = inverse)
}

print.bestNormalize <- function(BNobject) {
  
  prettyD <- paste0(
    'Estimated Normality Statistics (D):\n',
    ifelse(length(BNobject$D['boxcox']), 
           paste(" - Box-Cox:", round(BNobject$D['boxcox'], 4), '\n'), ''),
    ifelse(length(BNobject$D['boxcox']), 
           paste(" - Lambert's W:", round(BNobject$D['lambert'], 4), '\n'), ''),
    ifelse(length(BNobject$D['lambert']), 
           paste(" - Yeo-Johnson:", round(BNobject$D['yj'], 4), '\n'), '')
  )
  
  cat('Best Normalizing transformation with', BNobject$n, 'Observations\n',
      prettyD, '\nBased off these, bestNormalize chose:\n')
  print(BNobject$chosen_transform)
}
