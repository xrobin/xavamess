#' Test for near equality of two vectors
#' @description
#' This function is a vectorized version of \code{\link{all.equal}}. Instead of checking if all elements are nearly equal, it does the comparison element-wise like \code{==}. Arguments are recycled according to the standard rules.
#' @param x,y atomic vectors
#' @examples
#' a <- c(0.1, 0.2, 0.3) + 0.1
#' b <- c(0.2, 0.3, 0.4)
#' a == b
#' a %==% b
#' @aliases equal equality
#' @export
`%==%` <- function(x, y) {
  return(mapply(function(a, b) isTRUE(all.equal(a, b)), x, y))
}
