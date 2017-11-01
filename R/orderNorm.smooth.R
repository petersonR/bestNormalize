orderNorm.smooth <-
function(x, ties_method = c('warn', 'jitter')) {
    
    if(length(ties_method) == 2) ties_method <- ties_method[1]
    warn_status <- 0
    
    if (length(unique(x)) < length(x)) {
        if (ties_method == 'jitter') {
            tieIDX <- duplicated(x)
            noise <- diff(range(x, na.rm = T)) / 1000
            x[tieIDX] <- x[tieIDX] + rnorm(sum(tieIDX), 0, noise)
        } else {
            warning('Ties in data, Normal distribution not guaranteed')
            warn_status <- 1
        }
            
    }
    
    raw_x.t <- qnorm(rank(x, na.last = 'keep') / (length(x) + 1))
    s_fit <- mgcv::gam(raw_x.t ~ s(x))
    x.t <- unname(predict(s_fit))
    list(x.t = x.t,
         x = x, 
         n = length(x), 
         ties_method = ties_method, 
         method = 'orderNorm', 
         warn_status = warn_status)
}
