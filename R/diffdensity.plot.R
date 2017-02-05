#' Plot the difference of densities between two distributions 1 and 2
#' @param x1,x2,y1,y2 the x and y values for distributions 1 and 2.
#' @param bw a bandwidth selection function such as \code{\link{bw.nrd}}; or a numeric vector for \code{\link{kde2d}}.
#' @param n the number of grid points.
#' @param ztransform transformation of the z axis of the 2D density. The default \code{\link{sqrt_transform}} will take square root the differences in \code{z} to increase contrast. Use \code{\link{I}} in order to not transform the axis.
#' @param zlim overrides the limits in the z direction, setting the value where either \code{col1} or \code{col2} is displayed in full brightness.
#' @param zlim.mult a multiplication factor for zlim, especially useful in \code{relative = TRUE} mode.
#' @param plot.d1,plot.d2 whether to plot the 2d densities of distributions 1 and 2, respectively. Disabled by default.
#' @param plot.diff whether to plot the differential densities. Enabled by default.
#' @param draw.contour whether to overlay a \code{\link{contour}} on the diff plot. Ignored if \code{plot.diff = FALSE}.
#' @param contour.levels levels at which to draw contour lines, see the \code{levels} argument to \code{\link{contour}}
#' @param xlim,ylim limits of the plot on x and y.
#' @param extendrange,extendrange.x,extendrange.y extend \code{xlim} and \code{ylim} by a this fraction. Ignored if \code{xlim} and \code{ylim} are provided.
#' @param col1,col2 colors to represent the two distributions.
#' @param bg.col the background color.
#' @param ncol the number of colors of the palette. Should be an odd number to avoid weird effects around 0
#' @param relative whether to adjust \code{zlim} to match the highest density in distribution 1 and 2 (\code{TRUE}) or to the highest difference (\code{FALSE}). Ignored if \code{zlim} is provided.
#' @param add if TRUE, add to the current plot instead of creating a new one.
#' @param ... further arguments from and two other methods, in particular graphical parameters for \code{\link{image}} such as \code{main}, \code{xlab} or \code{ylab}.
#' @return
#' \code{\link[=invisible]{Invisibly}}, a \code{\link{list}} like \code{\link{kde2d}} with \code{z} the difference in densities, and the following additional \dQuote{z} elements:
#' \itemize{
#' \item{d1.z, d2.z}{Densities of distributions 1 and 2}
#' \item{sum.z}{The sum of the density distributions 1 and 2}
#' }
#' @seealso \href{http://stackoverflow.com/questions/28521145/r-calculate-and-plot-difference-between-two-density-countours}{The Stack Overflow answer} that inspired this function
#' @importFrom MASS kde2d
#' @importFrom grDevices colorRampPalette extendrange
#' @importFrom graphics image contour
#' @examples
#' # Get some gaussian distributions
#' x1 <- rnorm(10000)
#' y1 <- rnorm(10000)
#' x2 <- rnorm(10000, mean = 1, sd = 0.5)
#' y2 <- rnorm(10000, mean = 0.5, sd = 2)
#' # First shot at the plot
#' diffdensity.plot(x1, x2, y1, y2)
#'
#' # Make it smoother
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10)
#'
#' # Improve contour
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
#' 		contour.levels = c(0.01, 0.1, 0.2, 0.3, 0.4))
#'
#' # Change the colors
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
#' 		col1 = "green", col2 = "yellow", bg.col = "black")
#'
#' # Also plot d1 and d2
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
#' plot.d1 = TRUE, plot.d2 = TRUE, relative = TRUE)
#'
#' # Increase resolution
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
#' 		n = 500)
#'
#' # Try an assymetric distribution
#' y1 <- rexp(10000, rate = 1)
#' y2 <- rexp(10000, rate = 1.5)
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10)
#'
#' # A bit off the density is clipped in y, so extend the limits
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10, extendrange.y = .1)
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10, extendrange = .1, extendrange.x = 0)
#' # Extend in x as well
#' diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10, extendrange = .1)
#'
#' @export
diffdensity.plot <- function(x1, x2, y1, y2, bw = bw.nrd, n = 100,
							 ztransform = sqrt_transform, zlim = NULL, zlim.mult = 1, relative = FALSE,
							 plot.d1 = FALSE, plot.d2 = FALSE, plot.diff = TRUE,
							 draw.contour = TRUE, contour.levels = NULL,
							 xlim = extendrange(range(c(x1, x2)), f = extendrange.x),
							 ylim = extendrange(range(c(y1, y2)), f = extendrange.y),
							 extendrange = 0, extendrange.x = extendrange, extendrange.y = extendrange,
							 col1 = "blue", col2 = "red", bg.col = "white", ncol = 101, add = FALSE, ...) {

	if (ncol %% 2 == 0) warning("'ncol' should be an odd number")

	# Compute an average bandwidth
	if (is.function(bw)) {
		bw <- c(
			mean(c(bw(x1), bw(x2))),
			mean(c(bw(y1), bw(y2)))
		)
	}

	d1 = kde2d(x1, y1, lims=c(xlim, ylim), n=n, h = bw)
	d2 = kde2d(x2, y2, lims=c(xlim, ylim), n=n, h = bw)

	# Confirm that the grid points for each density estimate are identical
	stopifnot(identical(d1$x, d2$x))
	stopifnot(identical(d1$y, d2$y))

	# Calculate the difference between the 2d density estimates
	diffd = d1
	diffd$z = ztransform(d2$z - d1$z)
	# And the sum for the contours
	sumd = d1
	sumd$z = ztransform((d2$z + d1$z))

	# Apply z transformation to d1 and d2 too
	d1$z <- ztransform(d1$z)
	d2$z <- ztransform(d2$z)

	if (!is.null(zlim)) {
		if (relative) stop("'relative' argument ignored if 'zlim' is provided")
		zlim.d1 <- zlim.d2 <- zlim  <- zlim * zlim.mult
	}
	else if (relative) {
		zlim <- max(abs(range(d1$z, d2$z))) * c(-1, +1) * zlim.mult
		zlim.d1 <- range(d1$z) * zlim.mult
		zlim.d2 <- range(d2$z) * zlim.mult

	} else {
		zlim <- max(abs(range(diffd$z))) * c(-1, +1) * zlim.mult
		zlim.d1 <- range(d1$z) * zlim.mult
		zlim.d2 <- range(d2$z) * zlim.mult
	}

	if (plot.d1) {
		image(d1, col = colorRampPalette(c(bg.col, col1))(ncol), add = add, zlim = zlim.d1, ...)
	}
	if (plot.d2) {
		image(d2, col = colorRampPalette(c(bg.col, col2))(ncol), add = add, zlim = zlim.d2, ...)
	}
	if (plot.diff) {
		image(diffd, col = colorRampPalette(c(col1, bg.col, col2), alpha = TRUE)(ncol), zlim = zlim, add = add, ...)

		if (draw.contour) {
			if (is.null(contour.levels)) {
				contour.levels <- pretty(range(sumd$z), 5)
			}
			contour(sumd, levels = contour.levels, add = TRUE, drawlabels = FALSE, ...)
		}
	}

	diffd$sum.z <- sumd$z
	diffd$d1.z <- d1$z
	diffd$d2.z <- d2$z
	invisible(diffd)
}
