predict.orderNorm <- function(orderNorm.obj,
                              newdata = NULL,
                              inverse = FALSE) {
  
  if (is.null(newdata) & !inverse)
    newdata <- orderNorm.obj$x
  if (is.null(newdata))
    
  
  fitName <- ifelse(inverse, 'rev_s_fit', 's_fit')
  
  newdata <- data.frame(newdata)
  names(newdata) <- ifelse(inverse, 'raw_x.t', 'x')
  
  vals <-
    mgcv::predict.gam(orderNorm.obj[[fitName]], newdata = newdata)
  unname(as.vector(vals))
}
