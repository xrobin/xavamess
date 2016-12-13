#' @title Transformations
#' @description
#' Various scale transformation functions. These will keep the sign of x (and are basically \code{sign(x) * FUN(abs(x))})
#' @param x the data to transform
#' @rdname transformations
#' @name transformations
#' @aliases transform

#' @examples
#' curve(sqrt.transform(x), -2, 2, n = 1000)
#' @export sqrt.transform
#' @usage sqrt.transform(x)
sqrt.transform <- function(x) sign(x) * sqrt(abs(x))

#' @rdname transformations
#' @param exp the exponent for the transoformation x^exp
#' @examples
#' curve(power.transform(x), -2, 2, n = 1000)
#' curve(power.transform(x, 3), -2, 2, n = 1000)
#' @rdname transformations
#' @export power.transform
power.transform <- function(x, exp = 1/2) sign(x) * abs(x)^exp

#' @rdname transformations
#' @param base the logarithm base
#' @examples
#' curve(log.transform(x), -2, 2, n = 1000)
#' curve(log.transform(x, 10), -2, 2, n = 1000)
#' @export log.transform
#' @usage log.transform(x, base = exp(1))
log.transform <- function(x, base = exp(1)) sign(x) * log(abs(x), base = base)

#' @rdname transformations
#' @param FUN the transformation function, defined on the range [0, +Inf]
#' @examples
#' curve(FUN.transform(x, FUN = function(x) x + 2 * x^3), -2, 2)
#' curve(FUN.transform(x, FUN = function(x, a, b) x + a * x^b, a = 2, b = 3), -2, 2)
#' curve(FUN.transform(x, FUN = function(x, a, b) x + a * x^b, a = 4, b = 5), -2, 2)
#' @export
FUN.transform <- function(x, FUN, ...) sign(x) * FUN(abs(x), ...)
