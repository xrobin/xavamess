#' *apply with auto-parallelization if available
#' autoPar*apply functions guess the number of cores to use and
#' run in parallel if possible.
#' @rdname autoParApply
#' @param X,FUN,... arguments to \code{\link{lapply}}
#' @importFrom parallel parLapply
#' @examples
#' autoParLapply(1:10, function(x) x^2)
#' @export
autoParLapply <- function(X, FUN, ...) {
	return(autoParXapply(list(lapply, parLapply), X=X, FUN=FUN, ...))
}


#' @rdname autoParApply
#' @examples
#' autoParSapply(1:10, function(x) x^2)
#' @export
autoParSapply <- function(X, FUN, ...) {
	return(autoParXapply(list(sapply, parSapply), X=X, FUN=FUN, ...))
}


# This is the generic function
# It takes an extra argument .XFUN which is a list
# containing the two functions to use
# eg autoParXapply(list(sapply, parSapply), ...) for sapply
# ... contains MARGIN and FUN
autoParXapply <- function(.XFUN, X, ...) {
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(), length(X))
	if (ncpus > 1) {
		if (!require(parallel)) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
		ret <- .XFUN[[2]](cl = cl, X, ...)
		stopCluster(cl)
		return(ret)
	}
	else {
		return(.XFUN[[1]](X, FUN, ...))
	}
}
