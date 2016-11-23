#' Load to a list of environment
#' Loads an R dataset written with the \code{\link{save}} function into a \code{\link{list}} or \code{\link{environment}}
#' @rdname loadTo
#' @name loadTo
#' @param file the \code{\link{file}} argument for \code{\link{load}}, see there for more details
#' @return a list or environment with the named contents of \code{\link{file}}
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

#' @rdname loadTo
#' @examples
#' e <- loadToEnv(fname)
#' \dontshow{
#' testEnv <- new.env()
#' assign("a", "b", envir = testEnv)
#' assign("b", 5, envir = testEnv)
#' # Can't use identical on environment
#' stopifnot(isTRUE(all.equal(e, testEnv)))
#' }
#' file.remove(fname)
#' @export
loadToEnv <- function(file) {
	env <- new.env(parent = .GlobalEnv)
	loaded.object.names <- load(file, envir = env)
	return(env)
}
