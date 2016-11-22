#' *apply with auto-parallelization if available
#' autoPar*apply functions guess the number of cores to use and
#' run in parallel if possible.
#' @rdname autoParApply
#' @param X,MARGIN,FUN,... arguments to \code{\link{lapply}}, \code{\link{sapply}} and \code{\link{apply}}
#' @importFrom parallel parLapply clusterExport makeCluster parApply parSapply stopCluster
#' @importFrom stringr str_detect str_match
#' @examples
#' autoParLapply(1:10, function(x) x^2)
#' @export
autoParLapply <- function(X, FUN, ...) {
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE), length(X))
	if (ncpus > 1) {
		if (!requireNamespace("parallel")) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		prepareNodes(cl)
		#clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
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
		if (!requireNamespace("parallel")) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		prepareNodes(cl)
		#clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
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
		if (!requireNamespace("parallel")) {
			stop("parallel package required with more than 1 cores")
		}
		cl <- makeCluster(ncpus)
		# Export all .GlobalEnv
		prepareNodes(cl)
		#clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)
		ret <- parApply(cl = cl, X, MARGIN, FUN, ...)
		stopCluster(cl)
		return(ret)
	}
	else {
		return(apply(X, MARGIN, FUN, ...))
	}
}

# This function "prepares" the nodes for execution
# It loads all the packages currently loaded (in the search space)
# trying to keep their order
# It also sends the contents of the global env to the nodes
prepareNodes <- function(cl) {
	g <- globalenv()

	clusterExport(cl, ls(envir = g), envir = g)

	envs <- c()
	# Get all the environments
	while ((en <- environmentName(g)) != 'Autoloads') { # Exit when we reach Autoloads (then base then emptyenv)
		envs <- c(envs, en)
		g <- parent.env(g)
	}

	# Now start from the last ones
	for (en in rev(envs)) {
		if (str_detect(en, "^package:")) {
			packageName <- str_match(en, "^package:(.+)")[,2]
			cmd <- paste("clusterEvalQ(cl, library(", packageName, "))")
			eval(parse(text = cmd))
		}
		# For now we don't do anything for non-packages, but we could do something like this
		# except that we could end up overriding variables in the global env
		# else {
		# 	# Warning: everything ends in the global env this way...
		# 	message(sprintf("Exporting contents of %s to the global environment of the nodes", en))
		# 	suppressWarnings(clusterExport(cl, ls(envir = g), envir = g))
		#
		# }
	}
}
