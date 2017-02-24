library(xavamess)

context("calcModifiedPeptideP works")

test_that("calcModifiedPeptideP deals with phospho only", {
	p = calcModifiedPeptideP("_(ac)AGDS(ph)DSWDADAFSVEDPVRK_", "AGDS(1)DSWDADAFSVEDPVRK", "")
	expect_equal(p, 1)

	p = calcModifiedPeptideP("AGDS(ph)DSWDADAFSVEDPVRK_", "AGDS(0.900)DSWDADAFS(0.1)VEDPVRK", "")
	expect_equal(p, 0.9)
})

test_that("calcModifiedPeptideP deals with ox only", {
	p = calcModifiedPeptideP("_AAFNSGKVDIVAINDPFIDLNYM(ox)VYM(ox)FQYDSTHGK_", "", "AAFNSGKVDIVAINDPFIDLNYM(1)VYM(1)FQYDSTHGK")
	expect_equal(p, 1)
	p = calcModifiedPeptideP("_AAFNSGKVDIVAINDPFIDLNYM(ox)VYM(ox)FQYDSTHGK_", "", "AAFNSGKVDIVAINDPFIDLNYM(1)VYM(0.9)FQYDSTHGK")
	expect_equal(p, 0.9)
})


test_that("calcModifiedPeptideP deals with both phospho and ox only", {
	p = calcModifiedPeptideP("_AAEM(ox)CY(ph)RK_", "AAEMCY(0.99)RK", "AAEM(0.87)CYRK")
	expect_equal(p, 0.8613)
})


test_that("calcModifiedPeptideP deals with phospho at the end of the sequence", {
	p = calcModifiedPeptideP("_(ac)AGDS(ph)DSWDADAFSVEDPVRS(ph)_", "AGDS(0.995)DSWDADAFS(0.007)VEDPVRS(0.998)", "")
	expect_equal(p, 0.99301)
})


test_that("calcModifiedPeptideP deals with ox at the end of the sequence", {
	p = calcModifiedPeptideP("_(ac)AGDS(ph)DSWDADAFSVEDPVRM(ox)_", "AGDS(0.995)DSWDADAFS(0.007)VEDPVRM", "AGDSDSWDADAFSVEDPVRM(0.998)")
	expect_equal(p, 0.99301)
})


test_that("calcModifiedPeptideP handles missing probabilities", {
	p = calcModifiedPeptideP("_M(ox)LPHAPGVQM(ox)QAIPEDAIPEES(ph)GDEDEDDPDK_", "", "")
	expect_equal(p, 0)
})


test_that("calcModifiedPeptideP deals with vectors", {
	example <- matrix(c("_(ac)AGDS(ph)DSWDADAFSVEDPVRK_", "AGDS(1)DSWDADAFSVEDPVRK", "",
	"AGDS(ph)DSWDADAFSVEDPVRK_", "AGDS(0.900)DSWDADAFS(0.1)VEDPVRK", "",
	"_AAFNSGKVDIVAINDPFIDLNYM(ox)VYM(ox)FQYDSTHGK_", "", "AAFNSGKVDIVAINDPFIDLNYM(1)VYM(1)FQYDSTHGK",
	"_AAFNSGKVDIVAINDPFIDLNYM(ox)VYM(ox)FQYDSTHGK_", "", "AAFNSGKVDIVAINDPFIDLNYM(1)VYM(0.9)FQYDSTHGK",
	"_AAEM(ox)CY(ph)RK_", "AAEMCY(0.99)RK", "AAEM(0.87)CYRK",
	"_(ac)AGDS(ph)DSWDADAFSVEDPVRS(ph)_", "AGDS(0.995)DSWDADAFS(0.007)VEDPVRS(0.998)", "",
	"_(ac)AGDS(ph)DSWDADAFSVEDPVRM(ox)_", "AGDS(0.995)DSWDADAFS(0.007)VEDPVRM", "AGDSDSWDADAFSVEDPVRM(0.998)"),
	ncol = 3, byrow = TRUE)
	p = calcModifiedPeptideP(example[,1], example[,2], example[,3])

	expected.p <- c(1, .9, 1, .9, 0.8613, 0.99301, 0.99301)
	expect_equal(p, expected.p)
})
