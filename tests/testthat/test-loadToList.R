context("loadToList")

# Create a saved file to test
tmpfile <- mktemp()
a <- 1
b <- "test"
c <- list(a = a, b = b, c = c)
save(a, b, c, file = tmpfile)

test_that("loadToList works", {
	l <- loadToList(tmpfile)
	expect_identical(l, list(a = a, b = b, c = c))
})

test_that("loadToEnv works", {
	e <- loadToEnv(tmpfile)
	expected_e <- as.environment(list(a = a, b = b, c = c))
	expect_equal(e, expected_e)
})

file.remove(tmpfile)
