#' *apply with auto-parallelization if available
#'
#' autoPar*apply functions guess the number of cores to use and
#' run in parallel if possible. \dQuote{L}, \dQuote{S} and \dQuote{} (nothing) are available.
#' This function will make all objects in the global environment, and all currently loaded packages available to the parallel cluster nodes. This may fail in unexpected ways if other environments are \link[=attach]{attached} to the search path.
#' @rdname autoParApply
#' @param X,MARGIN,FUN,... arguments to \code{\link{lapply}}, \code{\link{sapply}} and \code{\link{apply}} or their parallel equivalents
#' @param .maxCores limit the number of cores.
#' @importFrom parallel parLapply clusterExport makeCluster parApply parSapply stopCluster
#' @importFrom stringr str_detect str_match
#' @examples
#' \dontrun{autoParLapply(1:10, function(x) x^2)}
#' @export
autoParLapply <- function(X, FUN, .maxCores = NULL, ...) {
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE, max = .maxCores), length(X))
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
#' \dontrun{autoParSapply(1:10, function(x) x^2)}
#' @export
autoParSapply <- function(X, FUN, .maxCores = NULL, ...) {
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE, max = .maxCores), length(X))
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
#' \dontrun{
#' x <- array(1:30, dim=c(2, 5, 3))
#' autoParApply(x, 1, sum)
#' autoParApply(x, c(1, 3), sum)
#' }
#' @export
autoParApply <- function(X, MARGIN, FUN, .maxCores = NULL, ...) {
	# What is length(X)?
	l <- prod(dim(X)[MARGIN])
	# Do not start more than length(X) workers
	ncpus <- min(guessCores(verbose = FALSE, max = .maxCores), l)
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
			cmd <- paste("parallel::clusterEvalQ(cl, library(", packageName, "))")
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
