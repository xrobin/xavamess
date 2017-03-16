context("equal")

a <- c(0.1, 0.2, 0.3) + 0.1
b <- c(0.2, 0.3, 0.4)

test_that("a and b differ with standard ==", {
	expect_identical(a == b, c(TRUE, FALSE, TRUE))
})

test_that("a and b are nearly equal with %==%", {
	expect_identical(a %==% b, rep(TRUE, 3))
})

test_that("a and b finds actual differences %==%", {
	c <- a
	c[1] <- c[1] - 0.01
	c[3] <- c[3] + 0.01
	expect_identical(a %==% c, c(FALSE, TRUE, FALSE))
})
