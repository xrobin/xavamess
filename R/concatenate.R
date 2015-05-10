#' @title Concatenate two character strings without any whitespace.
#' @description This is strictly equivalent to a call to paste0, with a bit less typing, similar to many other progamming languages that use \code{+} (or sometimes \code{.}) to concatenate strings. This function will take any type of vector happily, but make sense only with character vectors of length 1.
#' @param x,y the strings to concatenate
#' @return the concatenated string
#' @examples
#' "Hello" %+% " World!"
#' @export

"%+%" <- function(x, y) {
  return(paste0(x, y))
}