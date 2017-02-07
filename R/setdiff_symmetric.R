#' Symmetric setdiff
#' @description performs \emph{symmetric} set difference
#' @details
#' The \code{\link[base]{setdiff}} function from the \pkg{base} package performs assymetric difference of its arguments and returns the members of \code{x} missing in \code{y}. This function performs a symmetric difference instead, where both members of \code{x} missing in \code{y} and members of \code{y} missing in \code{x} will be returned.
#'
#' If \pkg{dplyr} is present, \code{\link{data.frame}}s can also be used as input.
#' @aliases sets
#' @param x,y objects to be compared
#' @examples
#' setdiff_symmetric(1:10, 5:15)
#' @export
#' @importFrom methods is
setdiff_symmetric <- function(x, y) {
	if (requireNamespace("dplyr")) {
		union <- dplyr::union
		setdiff <- dplyr::setdiff
	}
	else if (methods::is(x, "data.frame") || methods::is(y, "data.frame")) {
		stop("dplyr must be installed to handle data.frame input")
	}
	union(setdiff(x, y), setdiff(y, x))
}
