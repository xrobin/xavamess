library(xavamess)


context("autoParLapply returns the same results as lapply")
expect_identical(
	lapply(1:10, sqrt),
	autoParLapply(1:10, sqrt, .maxCores = 2)
)

context("autoParSapply returns the same results as sapply")
expect_identical(
	sapply(1:10, sqrt),
	autoParSapply(1:10, sqrt, .maxCores = 2)
)

context("autoParApply returns the same results as aapply")
x <- array(1:30, dim=c(2, 5, 3))
expect_identical(
	apply(x, 1, sum),
	autoParApply(x, 1, sum, .maxCores = 2)
)
expect_identical(
	apply(x, c(1, 3), sum),
	autoParApply(x, c(1, 3), sum, .maxCores = 2)
)


context("autoPar*apply finds stuff in the global environment")
# Make sure the next tests will fail if aVariableInGlobalEnv isn't  defined
expect_error(sapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}))
expect_error(lapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}))
expect_error(apply(x, 1, function(x) {isTRUE(aVariableInGlobalEnv)}))
expect_error(autoParSapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}), .maxCores = 2)
expect_error(autoParLapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}), .maxCores = 2)
expect_error(autoParApply(x, 1, function(x) {isTRUE(aVariableInGlobalEnv)}), .maxCores = 2)

aVariableInGlobalEnv <- TRUE
expect_identical(autoParSapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}, .maxCores = 2), c(TRUE, TRUE))
expect_identical(autoParLapply(1:2, function(x) {isTRUE(aVariableInGlobalEnv)}, .maxCores = 2), list(TRUE, TRUE))
expect_identical(autoParApply(x, 1, function(x) {isTRUE(aVariableInGlobalEnv)}, .maxCores = 2), c(TRUE, TRUE))
