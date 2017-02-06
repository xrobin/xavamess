#' Converts a letters-based representation into normal base 10 representation
#' @description In effect, this reverses the effect of \href{https://linux.die.net/man/1/split}{split} where output files are numbered with letters. This can be used to convert from other spaces too.
#' @param ... objects to be converted
#' @param space the original space, such as \code{\link{letters}}, \code{\link{LETTERS}}  or \code{1:10} (which will do nothing)
#' @examples
#' lettersToDecimal("aaaa")
#' lettersToDecimal("bczf")
#'
#' # Using the space of uppercase letters
#' lettersToDecimal("BCFZ", space = LETTERS)
#'
#' # To work with mixed case, use tolower or similar
#' lettersToDecimal(tolower("Bczf"))
#'
#' # Multiple inputs, the following lines are equivalent:
#' lettersToDecimal("aaaa", "bczf")
#' lettersToDecimal(c("aaaa", "bczf"))
#' @export
lettersToDecimal <- function(..., space = letters) {
	if (any(sapply(list(...), is.list))) stop("List arguments are not implemented yet")
	s <- strsplit(c(...), "")
	sapply(s, function(y, space) { # over elements of x
		decimal <- match(y, space)
		# aaaa means 0001 so we need to subtract 1 to anything but the last element
		decimal[- length(decimal)] <- decimal[- length(decimal)] - 1
		multiplier <- length(space) ^ (rev(seq(length(decimal))) - 1)
		multiplier[- length(decimal)]
		sum(decimal * multiplier)
	}, space = space, simplify = TRUE)
}
