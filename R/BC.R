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
