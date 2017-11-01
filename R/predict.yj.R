predict.yj <-
function(YJ.obj, newdata = NULL, inverse = F) {
    
    if(is.null(newdata) & !inverse) newdata <- YJ.obj$x
    if(is.null(newdata)) newdata <- YJ.obj$x.t
    
    if(inverse) {
        newdata = newdata * YJ.obj$std + YJ.obj$mean
        newdata = VGAM::yeo.johnson(newdata, YJ.obj$yj, inverse = T)
    } else if(!inverse) {
        newdata = VGAM::yeo.johnson(newdata, YJ.obj$yj)
        newdata = (newdata - YJ.obj$mean) / YJ.obj$std
    }
    
    unname(newdata)
}
