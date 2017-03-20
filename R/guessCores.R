#' Guesses a number of CPU to use for parallel calls
#'
#' First checks the environment variable \code{NCPUS}. This will typically be set in PBS jobs to the number of cores assigned to the job.
#' Otherwise, it will use the number of CPUs with \code{\link[parallel]{detectCores}}. If that fails or returns more than 12 cores, it will go back to 1 (as this is indicative that the job is being run on a supercomputer but without the NCPUS properly set).
#'
#' This function is especially useful for use with \code{\link[parallel]{makeCluster}}.
#' @param verbose show the number of cores that was guessed.
#' @param max limit the number of cores. Ignored if \code{NCPUS} was set.
#' @return a number of cores to use
#' @importFrom parallel detectCores
#' @examples
#' ncpus <- guessCores(max = 2)
#' # With the parallel package
#' library(parallel)
#' cl = makeCluster(ncpus)
#' parLapply(cl = cl, 1:ncpus, log)
#' stopCluster(cl)
#' @export
guessCores <- function(verbose = TRUE, max = NULL) {
	# Try the NCPUS env var
	ncpus <- as.integer(Sys.getenv("NCPUS"))
	if (is.na(ncpus)) {
		# Try to detect the number of cores on the machine
		ncpus <- detectCores()
		if (is.na(ncpus)) {
			ncpus <- 1L # assume single cores
		}
		else if (!is.null(max) && ncpus > (max <- as.integer(max))) {
			ncpus <- max # No more than max
		}
		else if (ncpus > 12) {
			# Too many cores indicate a supercomputer, back to 1 core
			# (PBS sets $NCPUS anyway)
			ncpus <- 1L
		}
	}
	else {
		if (ncpus <= 0) {
			ncpus <- 1L
		}
	}
	if (verbose) cat(sprintf("Using %s CPUs\n", ncpus))
	return(ncpus)
}
