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
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE), length(X))
	if (ncpus > 1) {
		if (!require(parallel)) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
		ret <- parLapply(cl = cl, X, fun=FUN, ...)
		stopCluster(cl)
		return(ret)
	}
	else {
		return(lapply(X, FUN, ...))
	}
}


#' @rdname autoParApply
#' @examples
#' autoParSapply(1:10, function(x) x^2)
#' @export
autoParSapply <- function(X, FUN, ...) {
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE), length(X))
	if (ncpus > 1) {
		if (!require(parallel)) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
		ret <- parSapply(cl = cl, X, FUN=FUN, ...)
		stopCluster(cl)
		return(ret)
	}
	else {
		return(sapply(X, FUN, ...))
	}
}


#' @rdname autoParApply
#' @examples
#' x <- array(1:30, dim=c(2, 5, 3))
#' autoParApply(x, 1, sum)
#' autoParApply(x, c(1, 3), sum)
#' @export
autoParApply <- function(X, MARGIN, FUN, ...) {
	# What is length(X)?
	l <- prod(dim(X)[MARGIN])
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE), l)
	if (ncpus > 1) {
		if (!require(parallel)) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
		ret <- parApply(cl = cl, X, MARGIN, FUN, ...)
		stopCluster(cl)
		return(ret)
	}
	else {
		return(apply(X, MARGIN, FUN, ...))
	}
}
