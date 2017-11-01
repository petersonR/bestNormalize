BC <-
function(x) {
    l <- forecast::BoxCox.lambda(x, method = 'loglik')
    x.t <- as.vector(forecast::BoxCox(x, l))
    
    mu <- mean(x.t)
    sigma <- sd(x.t)
    x.t <- (x.t - mu) / sigma
    list(
        x.t = x.t,
        x = x, 
        mean = mu,
        sd = sigma,
        lambda = l,
        n = length(x.t), 
        method = 'bc'
    )
}

predict.bc <-
  function(BC.obj, newdata = NULL, inverse = F) {
    
    if(is.null(newdata) & !inverse) newdata <- BC.obj$x
    if(is.null(newdata) & inverse) newdata <- BC.obj$x.t
    
    if(inverse) {
      newdata = newdata * BC.obj$sd + BC.obj$mean
      newdata = forecast::InvBoxCox(newdata, BC.obj$lambda)
    } else if(!inverse) {
      newdata = forecast::BoxCox(newdata, BC.obj$lambda)
      newdata = (newdata - BC.obj$mean) / BC.obj$sd
    }
    unname(newdata)
  }
