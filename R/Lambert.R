Lambert <-
function(x,
                    type = 's') {
    obj <-  unname(LambertW::Gaussianize(x, type = type, return.tau.mat = T))
    x.t <- obj[[1]]
    tau.mat <- obj[[2]]
    
    mu <- mean(x.t)
    sigma <- sd(x.t)
    
    x.t_uncentered <- x.t
    x.t <- (x.t - mu) / sigma
    
    attributes(x.t) <- NULL
    
    list(
        x.t = unname(x.t),
        x = x, 
        mean = mu, 
        sd = sigma,
        tau.mat = tau.mat,
        n = length(x.t),
        type = type,
        method = 'lw'
    )
}
