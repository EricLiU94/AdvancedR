euclidean <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
#' Find the largest common divisor
#' 
#' @param a An integer
#' @param b An integer
#' @return The largest common divisor of \code{a} and \code{b}
#' @examples 
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
