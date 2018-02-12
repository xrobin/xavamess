#' Remove missing values with a warning
#'
#' This function behaves exactly like \code{\link{na.omit}}, except that it prints a warning if any missing value was removed
#' @param object an R object
#' @param ... additional arguments
#' @examples
#' DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
#' na.warn(DF)
#' @export
na.warn <- function(object, ...) {
	object <- na.omit(object, ...)
	if (!is.null(missing <- attr(object, "na.action"))) {
		warning(sprintf("removed %d missing values in object", length(missing)))
	}
	return(object)
}
