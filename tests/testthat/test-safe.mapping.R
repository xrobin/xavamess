library(xavamess)

context("safe.mapping works")

test_that("safe.mapping works on valid inputs", {
	a <- data.frame(A = LETTERS[1:5], B = 1:5)
	b <- data.frame(A = LETTERS[1:5], C = 15:11)
	safe <- safe.mapping(a, b)
	expect_equal(nrow(safe), nrow(a))
	expect_equal(nrow(safe), 5)
})

test_that("safe.mapping stops on invalid input", {
	a <- data.frame(A = LETTERS[1:5], B = 1:5)
	c <- data.frame(A = LETTERS[1:5], C = 20:11)

	# Make sure dplyr duplicates on unsafe input
	unsafe <- left_join(a, c)
	expect_equal(nrow(unsafe), nrow(c))
	expect_equal(nrow(unsafe), 10)

	expect_error(safe.mapping(a, c))
})
