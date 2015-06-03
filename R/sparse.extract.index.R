#' Computes a matrix index that extracts individual (i, j) elements instead of the standard where all columns in rows \code{i} and all rows in columns \code{j} are extracted.
#' Note that this is likely to NOT work on data frames
#' @param i,j the indices of the elements to extract. Must have the same length.
#' @param nrow,ncol the number of rows and colums of the index. currently, \code{ncol} is ignored with \code{byrow = FALSE}, \code{nrow} is ignored with \code{byrow = TRUE}
#' @param byrow only \code{FALSE} is supported for now
#' @param na.rm remove elements where i or j are \code{NA} 
#' @param quiet if \code{FALSE}, prints a \code{\link{message}} before removing the missing values. If \code{TRUE}, missing values are silently removed.
#' @return the vector index for the matrix
#' @examples
#' m <- matrix(1:25, 5, 5)
#' m[1:5, 1:5] # Returns the whole matrix
#' m[sparse.extract.index(1:5, 1:5, nrow = 5)] # Returns the diagonal only
#' m[c(1, 3, 5), c(2, 4, 4)]
#' m[sparse.extract.index(c(1, 3, 5), c(2, 4, 4), nrow = 5)] # Returns the diagonal only
#' @export
sparse.extract.index <- function(i, j, nrow, ncol, byrow = FALSE, na.rm = FALSE, quiet = TRUE) {
  if (na.rm && any(nas <- is.na(i + j))) {
    if (!quiet) {
      message("Removing " %+% sum(nas) %+% " NA values from index")
    }
    i <- i[!nas]
    j <- j[!nas]
  }
  
  if (length(i) != length(j)) {
    stop("i and j lengths differ")
  }
  if (!missing(ncol) && any(j > ncol)) {
    stop("Elements in j cannot be larger than ncol")
  }
  if (!missing(nrow) && any(i > nrow)) {
    stop("Elements in i cannot be larger than nrow")
  }
  
  if (isTRUE(byrow)) {
    stop("byrow = TRUE not supported yet")
  }
  else {
    col.offset <- (j - 1) * nrow
    return(i + col.offset)
  }
}
