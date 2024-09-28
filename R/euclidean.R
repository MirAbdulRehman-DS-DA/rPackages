#' Euclidean Algorithm
#'
#' This function calculates the greatest common divisor (GCD) of two integers.
#'
#' @param a First integer
#' @param b Second integer
#' @return The GCD of `a` and `b`
#' @examples
#' euclidean(123612, 13892347912)  # Returns 4
#' euclidean(100, 1000)            # Returns 100
#' @export
euclidean <- function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b))
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}
