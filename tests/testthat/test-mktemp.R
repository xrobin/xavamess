library(xavamess)

context("mktemp can create a basic temp file")
tmpfile <- mktemp()
expect_true(file.exists(tmpfile))
file.remove(tmpfile)
expect_false(file.exists(tmpfile))

context("mktemp rejects illegal templates")
illegal.templates <- c(NA, "", "a", "aa", "aaa", "aa-XXX", "aa-XXX.", "aa-XXX-")
expect_error(mktemp(illegal.templates))
lapply(illegal.templates, function(i) {
	expect_error(mktemp(i))
})

context("mktemp works with legal templates")
legal.templates <- c("XXX", "a.XXX")
lapply(legal.templates, function(i) {
	tmpfile <- mktemp(i)
	expect_true(file.exists(tmpfile))
	file.remove(tmpfile)
	expect_false(file.exists(tmpfile))
})