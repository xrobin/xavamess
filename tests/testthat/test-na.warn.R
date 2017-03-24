library(xavamess)

context("na.warn works")
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))

test_that("na.warn warns", {
	expect_warning(nona <- na.warn(DF), regexp = "1")
	expect_identical(nrow(nona), 2L)
})

test_that("na.warn is silent with no NAs", {
	expect_silent(na.warn(DF[1:2,]))
})
