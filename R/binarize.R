binarize <- function(x, location_measure = 'median') {
  if (is.numeric(location_measure)) {
    loc <- location_measure
  } else  if (location_measure == 'median') {
    loc <- median(x, na.rm = T)
  } else if (location_measure == 'mean') {
    loc <- mean(x, na.rm = T)
  } else if (location_measure == 'mode') {
    dens <- density(x)
    loc <- dens$x[which.max(dens$y)]
  }
  
  x.t <- as.numeric(x > loc)
  
  val <- list(
    x.t = x.t,
    x = x,
    method = location_measure,
    location = loc,
    n = length(x.t)
  )
  class(val) <- 'binarize'
  val
}

predict.binarize <- function(binarize.obj, newdata = NULL, inverse = F) {
  if (is.null(newdata) & !inverse)
    newdata <- binarize.obj$x
  if (is.null(newdata))
    newdata <- binarize.obj$x.t
  
  if (!inverse)
    return(as.numeric(newdata > binarize.obj$location))
  prettyLoc <- round(binarize.obj$location,
                     getOption('digits', 2))
  labels <- c(paste0('< ', prettyLoc),
              paste0('>= ', prettyLoc))
  factor(newdata, levels = 0:1, labels = labels)
}

print.binarize <- function(x) {
  cat('Binarize Transformation with', binarize.obj$n, 
      'observations\nEstimated Statistic:\n -', binarize.obj$method ,
      '=', binarize.obj$location)
}

