#' Generate a regular sequence from a range of values
#' 
#' @param range a numeric vector of value
#' @param ... arguments passed to or from methods, in particular \code{by} and \code{length.width} passed to \code{\link{seq}}.
#' @examples 
#' rangev <- rpois(5, 10)
#' seq_range(rangev)
#' # Warns with NA values
#' rangev.na <- c(NA, rangev)
#' seq_range(rangev.na)
#' 
#' @export
seq_range <- function(range, ...) {
  r <- range(na.warn(range))
  return(seq(from=r[1], to = r[2], ...))
}
