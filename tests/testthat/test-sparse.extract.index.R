library(xavamess)

context("sparse.extract.index works")

m <- matrix(1:30, 5, 6)

# diagonal
expect_identical(m[sparse.extract.index(1:5, 1:5, nrow = 5)] , diag(m))

# Arbitrary
expect_identical(m[sparse.extract.index(c(1, 3, 5), c(2, 4, 4), nrow = 5)], c(6L, 18L, 20L))

# Single
expect_identical(m[sparse.extract.index(5, 2, nrow = 5)], 10L)

# NA works
expect_identical(m[sparse.extract.index(c(2, NA, 4), c(2, 4, 5), nrow = 5, na.rm = TRUE)], c(7L, 24L))
expect_identical(m[sparse.extract.index(c(2, 3, 4), c(2, NA, 5), nrow = 5, na.rm = TRUE)], c(7L, 24L))


# test errors
context("sparse.extract.index rejects invalid input")
# Invalid i and j
expect_error(m[sparse.extract.index(5, 2:3, nrow = 5)])
expect_error(m[sparse.extract.index(2:5, 2:3, nrow = 5)])
expect_error(m[sparse.extract.index(c(1, NA), 2:3, nrow = 5)])
