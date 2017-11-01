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
