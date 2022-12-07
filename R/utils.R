#' Reverse scores
#' @param x an integer
#' @param max maximum scale score
#' @param eps 
#' @noRd
nice_reverse <- function(x, max, eps) {
  max - as.numeric(x) + eps
}