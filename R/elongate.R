#' @title Elongates a pseudo-wide column
#' @description Takes a pseudo-wide column and tranform it into a long format
#' @param id a column that will not be splitted
#' @param wide the column to split
#' @param pattern the character or regex used to split up the wide column (see \link{str_split})
#' @examples
#' data(pseudo.wide)
#' elongated.score <- elongate(pseudo.wide$Protein, pseudo.wide$Score)
#' elongated.sequence <- elongate(pseudo.wide$Protein, pseudo.wide$Sequence)
#' # The following works only if the Score and Sequence have the same pattern
#' # (ie same number of splits per ID)
#' elongated <- data.frame(Protein = elongated.score$id,
#'                         Score = elongated.score$wide,
#'                         Sequence = elongated.sequence$wide)
#' \dontshow{
#' stopifnot(identical(dim(elongated.score), dim(elongated.sequence)))
#' }
#' @importFrom stringr str_split
#' @export
elongate <- function(id, wide, pattern = ";") {
	splitted <- str_split(wide, pattern)
	long <- data.frame(id = rep.int(id, sapply(splitted, length)), wide = unlist(splitted))
	return(long)
}



