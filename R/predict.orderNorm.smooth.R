predict.orderNorm.smooth <-
function(orderNorm.obj, newdata = NULL, inverse = F) {
    if(is.null(newdata) & !inverse) newdata <- orderNorm.obj$x
    if(is.null(newdata)) newdata <- orderNorm.obj$x.t
    
    x <- orderNorm.obj$x
        s_fit <- mgcv::gam(orderNorm.obj$x.t ~ s(x))
       
    if(!inverse) {
        vals <- unname(predict(s_fit, newdata = data.frame(x = newdata)))
        return(vals)
    }
        
    vals <- revGam(s_fit, newdata = newdata)
    vals
}
