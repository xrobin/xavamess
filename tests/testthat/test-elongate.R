context("elongate")

options(stringsAsFactors = FALSE) #

elongated.score.expected <- structure(list(id = c("ENSP00000340878", "ENSP00000340878", "ENSP00000340878", "ENSP00000378183", "ENSP00000215582", "ENSP00000215582", "ENSP00000215582"), wide = c("69.48489", "21.03223", "21.00795", "141.1382", "106.9799", "11.8921", "5.311716")), .Names = c("id", "wide"), row.names = c(NA, -7L), class = "data.frame")

elongated.sequence.expected <- structure(list(id = c("ENSP00000340878", "ENSP00000340878", "ENSP00000340878", "ENSP00000378183", "ENSP00000215582", "ENSP00000215582", "ENSP00000215582"), wide = c("ALSSDSILSPAPDAR", "NYYHQVMKRSR", "NYLPIFPVMYSR", "ALSSEEEEEMGGAAQEPESLLPPSVLDQASVIAER", "ALSSDSILSPAPDAR", "QRPDGRSVSESLR", "APMKTPTMPPLGSR")), .Names = c("id", "wide"), row.names = c(NA, -7L), class = "data.frame")

test_that("elongate works as expected", {
	data(pseudo.wide)
	elongated.score <- elongate(pseudo.wide$Protein, pseudo.wide$Score)
	expect_identical(elongated.score, elongated.score.expected)
	elongated.sequence <- elongate(pseudo.wide$Protein, pseudo.wide$Sequence)
	expect_identical(elongated.sequence, elongated.sequence.expected)
})
