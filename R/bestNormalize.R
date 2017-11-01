bestNormalize <-
function(x,
                          norm_test = c('lillie'),
                          D_max = 1,
                          allow_orderNorm = T,
                          ...) {
    x.t <- list(lw = Lambert(x),
                bc = BC(x),
                yj = YJ(x))
    D <- rep(NA, 5)
    
    if (norm_test == 'lillie') {
        D[1:3] <- unlist(lapply(x.t, function(x)
            nortest::lillie.test(as.vector(x$x.t))$statistic))
        
    } else
        stop('norm_test method not recognized')
    
    if (min(D, na.rm = T) > D_max) {
        if (allow_orderNorm) {
            x.t$orderNorm <- orderNorm.smooth(x, ...)
            D[4] <- nortest::lillie.test(as.vector(x.t$orderNorm$x.t))$statistic
            norm_method <- 'orderNorm'
        } else if(D[4] > D_max) {
            x.t$binarize <- binarize(x, ...)
            norm_method <- 'binarize'
            D[5] <- -Inf
            warning('Insufficient Transformation,
                    \nincrease D_max? Check for binary variable?')
        }
    } 
    
    xFinal <- x.t[[which.min(D)]]$x.t
    norm_method <- names(x.t)[which.min(D)]
    names(D) <- c('lw' ,'bc', 'yj', 'orderNorm', 'binarize')
    list(method = norm_method,
         x.t = xFinal, 
         x = x,
         D = D,
         method_info = x.t[[which.min(D)]])
}
