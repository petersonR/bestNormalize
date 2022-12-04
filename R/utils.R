#' Reverse scores
#' @param x an integer
#' @param max maximum scale score
#' @param min minimum scale score
#' @noRd
nice_reverse <- function(x, max, min) {
  max - as.numeric(x) + min
}