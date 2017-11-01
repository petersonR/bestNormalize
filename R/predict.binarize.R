predict.binarize <-
function(binarize.obj, newdata = NULL, inverse = F) {
    if(is.null(newdata) & !inverse) newdata <- binarize.obj$x
    if(is.null(newdata)) newdata <- binarize.obj$x.t
    
    if(!inverse) return(as.numeric(newdata > binarize.obj$location))
    prettyLoc <- round(binarize.obj$location, 
                       getOption('digits', 2))
    labels <- c(paste0('< ', prettyLoc), 
                paste0('>= ', prettyLoc))
    factor(newdata, levels = 0:1, labels = labels)
}
