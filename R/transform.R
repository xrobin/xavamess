#' @title Transformations
#' @description
#' Various scale transformation functions. These will keep the sign of x (and are basically \code{sign(x) * FUN(abs(x))})
#' @param x the data vector to transform
#' @rdname transformations
#' @name transformations
#' @aliases transform

#' @examples
#' curve(sqrt_transform(x), -2, 2, n = 1000)
#' @export sqrt_transform
#' @usage sqrt_transform(x)
sqrt_transform <- function(x) sign(x) * sqrt(abs(x))

#' @rdname transformations
#' @param exp the exponent for the transoformation x^exp
#' @examples
#' curve(power_transform(x), -2, 2, n = 1000)
#' curve(power_transform(x, 3), -2, 2, n = 1000)
#' @rdname transformations
#' @export power_transform
power_transform <- function(x, exp = 1/2) sign(x) * abs(x)^exp

#' @rdname transformations
#' @param base the logarithm base
#' @examples
#' curve(log_transform(x), -2, 2, n = 1000)
#' curve(log_transform(x, 10), -2, 2, n = 1000)
#' @export log_transform
#' @usage log_transform(x, base = exp(1))
log_transform <- function(x, base = exp(1)) sign(x) * log(abs(x), base = base)

#' @rdname transformations
#' @param FUN the transformation function, defined on the range [0, +Inf]
#' @param ... additional arguments for \code{FUN}
#' @examples
#' curve(FUN_transform(x, FUN = function(x) x + 2 * x^3), -2, 2)
#' curve(FUN_transform(x, FUN = function(x, a, b) x + a * x^b, a = 2, b = 3), -2, 2)
#' curve(FUN_transform(x, FUN = function(x, a, b) x + a * x^b, a = 4, b = 5), -2, 2)
#' @export
FUN_transform <- function(x, FUN, ...) sign(x) * FUN(abs(x), ...)
