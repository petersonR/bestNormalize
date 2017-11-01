YJ <-
function(x, ...) {
    p.yj <- caret::preProcess(data.frame(x), 
                              method = c('YeoJohnson', 'center', 'scale'),
                              ...)
    p.yj$x.t <- predict(p.yj, newdata = data.frame(x))[,1]
    p.yj$x <- x
    p.yj
}
