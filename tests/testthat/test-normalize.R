context("normalize")

log.normal <- c(0.693432514003335, 0.780534411091866, 2.12243319701359, 0.664126177011066, 0.60770998961496, 1.04437003303986, 1.15905230657191, 4.00255336497087, 7.78404407428789, 0.445041131839186, 1.85878055532097, 0.209252159797218, 3.02542970673056, 2.09602738826939, 2.76661305808165, 0.912366301342495, 4.81749445223961, 3.47535102577474, 0.669766027494569, 1.87766114246149)


test_that("rank normalize works with default arguments" {
	norm <- rank.normalize(sort(log.normal))
	expect_identical(norm, qnorm((1:20)/21))

	# Without sort
	norm <- rank.normalize(log.normal)
	expect_identical(norm[order(log.normal)], qnorm((1:20)/21))
})

test_that("rank normalize works with custom function" {
	norm <- rank.normalize(sort(log.normal), FUN = qexp)
	expect_identical(norm, qexp((1:20)/21))
})


test_that("rank normalize croaks with NA and na.fail", {
	na.log.normal <- log.normal
	na.log.normal[10] <- NA
	expect_error(rank.normalize(na.log.normal, na.action = na.fail))
})
