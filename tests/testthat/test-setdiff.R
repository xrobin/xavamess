library(xavamess)

context("setdiff_symmetric works")

test_that("setdiff_symmetric works assymetrically", {
	expect_identical(setdiff_symmetric(1:10, 5:15), c(1:4, 11:15))
	expect_identical(setdiff_symmetric(letters[1:10], letters[5:15]), letters[c(1:4, 11:15)])
})

test_that("setdiff_symmetric works when only 1 object has additional elements", {
	expect_identical(setdiff_symmetric(1:10, 5:10), 1:4)
	expect_identical(setdiff_symmetric(letters[1:10], letters[5:10]), letters[1:4])

	expect_identical(setdiff_symmetric(5:10, 5:15), 11:15)
	expect_identical(setdiff_symmetric(letters[5:10], letters[5:15]), letters[11:15])
})

test_that("setdiff_symmetric returns an empty set for identical vectors", {
	expect_identical(setdiff_symmetric(1:10, 1:10), integer(0))
	expect_identical(setdiff_symmetric(letters[1:10], letters[1:10]), character(0))
})

test_that("setdiff_symmetric works with data.frames", {
	skip_if_not_installed("dplyr")
	# Create a dummy data.frame
	df <- data.frame(a = 1:15, b = letters[1:15])

	# Compute the set and the expected result
	res <- setdiff_symmetric(df[1:10,], df[5:15,])
	exp <- df[c(1:4, 11:15),]
	# The DFs may come in a different order and with different row names
	res <- res[order(res$b),]
	rownames(res) <- NULL
	exp <- exp[order(exp$b),]
	rownames(exp) <- NULL

	# Check
	expect_identical(res, exp)

	# Also check empty difference
	expect_identical(nrow(setdiff_symmetric(df[1:10,], df[1:10,])), 0L)

})
