#' Load an R dataset written with the \code{\link{save}} function into a \code{\link{list}}
#' @param file the \code{\link{file}} argument for \code{\link{load}}, see there for more details
#' @return a list with the named contents of \code{\link{file}}
#' @examples
#' fname <- tempfile()
#' a <- "b"
#' b <- 5
#' save(a, b, file=fname)
#' l <- loadToList(fname)
#' \dontshow{
#' stopifnot(identical(l, list(a="b", b=5)))
#' }
#' @export
loadToList <- function(file) {
	loaded.object.names <- load(file)
	ret <- list()
	for (object.name in loaded.object.names) {
		ret[[object.name]] <- get(object.name)
	}
	return(ret)
}
