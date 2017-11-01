predict.lw <-
function(LW.obj, newdata = NULL, inverse = F) {
    
    if(is.null(newdata) & !inverse) newdata <- LW.obj$x
    if(is.null(newdata) & inverse) newdata <- LW.obj$x.t
    
    if(inverse) newdata = newdata * LW.obj$sd + LW.obj$mean
    
    newdata <- LambertW::Gaussianize(
        as.matrix(newdata),
        type = LW.obj$type,
        tau.mat = LW.obj$tau.mat,
        inverse = inverse
    )
    
    if(!inverse) newdata = (newdata - LW.obj$mean) / LW.obj$sd
    attributes(newdata) <- NULL
    
    unname(newdata)
}
