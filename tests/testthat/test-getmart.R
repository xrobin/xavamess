library(xavamess)

context("getMart works")

test_that("getMart works on human inputs", {
	skip_if_not_installed("biomaRt")

	library(biomaRt)
	ensembl.proteins <- c("ENSP00000361930", "ENSP00000309503")
	mart <- getMart(88, "human")
	res <- getBM(attributes = c("ensembl_peptide_id", "uniprotswissprot", "refseq_peptide", "hgnc_symbol"), filters = "ensembl_peptide_id", values = ensembl.proteins, mart = mart)


	order.input <- order(ensembl.proteins)
	order.output <- order(res$ensembl_peptide_id)
	expect_equal(ensembl.proteins[order.input], res$ensembl_peptide_id[order.output])
	expect_equal(res$uniprotswissprot[order.output], c("P63104", "P31946"))
	expect_equal(res$refseq_peptide[order.output], c("NP_003397", "NP_003395"))
	expect_equal(res$hgnc_symbol[order.output], c("YWHAZ", "YWHAB"))
})

test_that("getMart works on mouse inputs", {
	skip_if_not_installed("biomaRt")

	library(biomaRt)
	ensembl.proteins <- c("ENSMUSP00000004055", "ENSMUSP00000002708")
	mart <- getMart(88, "mmusculus")
	mart.mouse <- getMart(88, "mouse")
	expect_identical(mart, mart.mouse)

	res <- getBM(attributes = c("ensembl_peptide_id", "uniprotswissprot", "refseq_peptide", "mgi_symbol"), filters = "ensembl_peptide_id", values = ensembl.proteins, mart = mart)

	order.input <- order(ensembl.proteins)
	order.output <- order(res$ensembl_peptide_id)
	expect_equal(ensembl.proteins[order.input], res$ensembl_peptide_id[order.output])
	expect_equal(res$uniprotswissprot[order.output], c("Q62226", "Q8BMD2"))
	expect_equal(res$refseq_peptide[order.output], c("NP_033196", "NP_080219"))
	expect_equal(res$mgi_symbol[order.output], c("Shh", "Dzip1"))
})

test_that("getMart crashes on in invalid species input", {
	skip_if_not_installed("biomaRt")
	expect_error(getMart(88, "nospecies"))
})
