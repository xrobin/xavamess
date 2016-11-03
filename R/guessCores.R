#' Guesses a number of CPU to use for parallel calls
#' Especially useful for use with \code{\link{makeCluster}}.
#' 
#' First checks the environment variable NCPUS. This will typically be set in PBS jobs to the number of cores assigned to the job.
#' Otherwise, will use the number of CPUs with \code{\link{parallel::detectCores}}. If that fails or returns more than 12 cores, it will go back to 1 (as this is indicative that the job is being run on a supercomputer but without the NCPUS properly set) 
#' @param verbose show the number of cores that was guessed
#' @return a number of cores to use
#' @importFrom parallel detectCores
#' @examples 
#' print(ncpus <- guessCores())
#' # With the parallel package
#' library(parallel)
#' cl = makeCluster(ncpus)
#' parLapply(cl = cl, 1:ncpus, log)
#' stopCluster(cl)
#' @export
guessCores <- function(verbose = TRUE) {
  # Try the NCPUS env var
  ncpus <- as.numeric(Sys.getenv("NCPUS"))
  if (is.na(ncpus)) {
    # Try to detect the number of cores on the machine
    ncpus <- detectCores()
    if (is.na(ncpus)) {
      ncpus <- 1 # assume single cores
    }
    else if (ncpus > 12) {
      # Too many cores indicate a supercomputer, back to 1 core
      # (PBS sets $NCPUS anyway)
      ncpus <- 1
    }
  }
  if (verbose) cat(sprintf("Using %s CPUs", ncpus))
  return(ncpus)
}
