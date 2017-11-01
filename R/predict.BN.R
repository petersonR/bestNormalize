predict.BN <-function(BNobject,
                      newdata = NULL,
                      inverse = F) {
  method = BNobject$method
  
  
  # Caret preprocess YJ object's method == 4
  if (length(method) == 4)
    method = 'yj'
  
  obj <- BNobject$method_info
  if (is.null(obj))
    obj <- BNobject
  
  if (method == 'yj') {
    newdata = predict.yj(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'lw') {
    newdata = predict.lw(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'bc') {
    newdata = predict.bc(obj,
                         newdata = newdata,
                         inverse = inverse)
  } else if (method == 'orderNorm') {
    newdata = predict.orderNorm(obj, 
                                newdata = newdata, 
                                inverse = inverse)
  } else if (method == 'binarize') {
    newdata = predict.binarize(obj,
                               newdata = newdata,
                               inverse = inverse)
  } else
    stop('method not recognized')
  
  # Add in more functions here for other methods
  
  newdata
}
