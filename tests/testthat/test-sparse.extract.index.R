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
context("sparse.extract.index removes NA")
expect_identical(m[sparse.extract.index(c(2, NA, 4), c(2, 4, 5), nrow = 5, na.rm = TRUE)], c(7L, 24L))
expect_identical(m[sparse.extract.index(c(2, 3, 4), c(2, NA, 5), nrow = 5, na.rm = TRUE)], c(7L, 24L))

# Expect some output
context("sparse.extract.index removes NA")
expect_silent(sparse.extract.index(c(2, NA, 4), c(2, 4, 5), nrow = 5, na.rm = TRUE))
expect_silent(sparse.extract.index(c(2, 3, 4), c(2, NA, 5), nrow = 5, na.rm = TRUE))
expect_message(sparse.extract.index(c(2, NA, 4), c(2, 4, 5), nrow = 5, na.rm = TRUE, quiet = FALSE))
expect_message(sparse.extract.index(c(2, 3, 4), c(2, NA, 5), nrow = 5, na.rm = TRUE, quiet = FALSE))

# test errors
context("sparse.extract.index rejects invalid input")
# Invalid i and j
expect_error(m[sparse.extract.index(5, 2:3, nrow = 5)])
expect_error(m[sparse.extract.index(2:5, 2:3, nrow = 5)])
expect_error(m[sparse.extract.index(c(1, NA), 2:3, nrow = 5)]) # NA without na.rm=TRUE

# Uneven i and j
expect_error(m[sparse.extract.index(5, 2:3, nrow = 4)])
expect_error(m[sparse.extract.index(5, 2:3)])

# Nrow/ncol missing or smaller than i/j
expect_error(sparse.extract.index(c(1, 5), 2:3, nrow = 4))
expect_error(sparse.extract.index(2:3, c(1, 5), ncol = 4))
