library(xavamess)

ncores <- guessCores()

context("guessCores returns sensible results")
expect_true(is.integer(ncores), label = "ncores is an integer")
expect_gt(ncores, 0, label = "ncores is positive")

context("guessCores output is sensible")
expect_output(guessCores(verbose = FALSE), NA, label = "no output with verbose=FALSE")
expect_output(guessCores(verbose = TRUE), sprintf(" %d ", ncores), label = "output contains the number of cores with verbose=TRUE")

context("guessCores's 'max' argument works")
expect_lte(guessCores(max = 2), 2, label = "returns no more than 2 with max = 2")
