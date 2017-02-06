library(xavamess)

context("letterToDecimal works")
expect_identical(lettersToDecimal("a"), 1)
expect_identical(lettersToDecimal("b"), 2)
expect_identical(lettersToDecimal("aa"), 1)
expect_identical(lettersToDecimal("aaa"), 1)
expect_identical(lettersToDecimal("aab"), 2)
expect_identical(lettersToDecimal("aaz"), 26)
expect_identical(lettersToDecimal("aba"), 27)
expect_identical(lettersToDecimal("aza"), 651)
expect_identical(lettersToDecimal("baa"), 677)
expect_identical(lettersToDecimal("zzz"), 17576)


context("letterToDecimal can take multiple arguments")
expect_identical(lettersToDecimal(c("a", "baa")), c(1, 677))
expect_identical(lettersToDecimal("a", "baa"), c(1, 677))

context("letterToDecimal rejects a list")
expect_error(lettersToDecimal(list("a", "baa")))


context("letterToDecimal can take different input spaces")
expect_identical(lettersToDecimal("a", space = letters[1:2]), 1)
expect_identical(lettersToDecimal("b", space = letters[1:2]), 2)
expect_identical(lettersToDecimal("aa", space = letters[1:2]), 1)
expect_identical(lettersToDecimal("aaa", space = letters[1:2]), 1)
expect_identical(lettersToDecimal("aab", space = letters[1:2]), 2)
expect_identical(lettersToDecimal("aba", space = letters[1:2]), 3)
expect_identical(lettersToDecimal("aaa", "aab", "aba", "abb", "baa", "bab", "bba", "bbb", space = letters[1:2]), as.numeric(1:8))


context("letterToDecimal can deal with input out of the space different input spaces")
expect_identical(lettersToDecimal("aza", space = letters[1:2]), NA_real_)
expect_identical(lettersToDecimal("aza", "aba", space = letters[1:2]), c(NA_real_, 3))
expect_identical(lettersToDecimal(c("aza", "aba"), space = letters[1:2]), c(NA_real_, 3))
