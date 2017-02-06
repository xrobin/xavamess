library(xavamess)

ncores <- guessCores(verbose = FALSE)

context("guessCores returns sensible results")
expect_true(is.integer(ncores), label = "ncores is an integer")
expect_gt(ncores, 0, label = "ncores is positive")

context("guessCores output is sensible")
expect_silent(guessCores(verbose = FALSE))
expect_output(guessCores(verbose = TRUE), sprintf(" %d ", ncores), label = "output contains the number of cores with verbose=TRUE")

context("guessCores's 'max' argument works")
expect_lte(guessCores(max = 2, verbose = FALSE), 2, label = "returns no more than 2 with max = 2")
