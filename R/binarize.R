binarize <-
function(x, location_measure = 'median') {
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
    
    results <- list(
        x.t = x.t, 
        x = x,
        method = location_measure, 
        location = loc, 
        method = 'binarize'
    )
}
