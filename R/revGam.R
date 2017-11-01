revGam <-
function(gam_fit, newdata = NULL) {
    vals <- numeric(length(newdata))
    root_range <- range(min(gam_fit$model$x) - 3 * diff(gam_fit$model$x), 
                        max(gam_fit$model$x) + 3 * diff(range(gam_fit$model$x)))
    
    for(i in 1:length(newdata)) {
        
        vals[i] <- uniroot(
            function(xxx) {
                predict(gam_fit, newdata = data.frame(x = xxx)) - newdata[i]
            },  interval = root_range, extendInt = 'yes')$root
    }
    vals
}
