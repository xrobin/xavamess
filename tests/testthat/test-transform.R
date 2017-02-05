library(xavamess)

context("sqrt_transform works")
expect_equal(sqrt_transform(c(-10, -1.1, 0, 1.1, 10)), c(-sqrt(10), -sqrt(1.1), 0, sqrt(1.1), sqrt(10)))

context("power_transform works")
# default power is 1/2 = sqrt
expect_equal(power_transform(c(-10, -1.1, 0, 1.1, 10)), c(-sqrt(10), -sqrt(1.1), 0, sqrt(1.1), sqrt(10)))
expect_equal(power_transform(c(-10, -1.1, 0, 1.1, 10), 3), c(-(10^3), -(1.1^3), 0, 1.1^3, 10^3))

context("log_transform works")
expect_equal(log_transform(c(-10, -1.1, -1, -0.01, 0.01, 1, 1.1, 10)), c(-log(10), -log(1.1), 0, -log(0.01), log(0.01), 0, log(1.1), log(10)))
expect_equal(log_transform(c(-10, -1.1, -1, -0.01, 0.01, 1, 1.1, 10), base = 10), c(-log10(10), -log10(1.1), 0, -log10(0.01), log10(0.01), 0, log10(1.1), log10(10)))
expect_equal(log_transform(c(-10, -1.1, -1, -0.01, 0.01, 1, 1.1, 10), base = 2), c(-log2(10), -log2(1.1), 0, -log2(0.01), log2(0.01), 0, log2(1.1), log2(10)))


context("FUN_transform works")
expect_equal(FUN_transform(c(-10, -1, 0, 1, 10), function(x) x+3), c(-13, -4, 0, 4, 13))
expect_equal(FUN_transform(c(-10, -1, 0, 1, 10), log), log_transform(c(-10, -1, 0, 1, 10)))
