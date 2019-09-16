#' Find the largest common divisor
#' 
#' @export euclidean
#' @param a An integer
#' @param b An integer
#' @return The largest common divisor of \code{a} and \code{b}
#' @examples 
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
euclidean <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm