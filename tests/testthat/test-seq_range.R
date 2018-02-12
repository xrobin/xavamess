library(xavamess)


test_that("seq_range generates correct range", {
	r <- c(2, 10)
	expect_equal(seq_range(r), 2:10)

	# with 0
	r <- c(0, 5)
	expect_equal(seq_range(r), 0:5)

	# negative values
	r <- c(-5, 5)
	expect_equal(seq_range(r), (-5):5)
})

test_that("seq_range warns upon NAs", {
	r <- c(NA, 2, 10)
	expect_warning(seq_range(r))
})

