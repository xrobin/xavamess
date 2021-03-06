% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffdensity.plot.R
\name{diffdensity.plot}
\alias{diffdensity.plot}
\title{Plot the difference of densities between two distributions 1 and 2}
\usage{
diffdensity.plot(
  x1,
  x2,
  y1,
  y2,
  bw = bw.nrd,
  n = 100,
  ztransform = sqrt_transform,
  zlim = NULL,
  zlim.mult = 1,
  relative = FALSE,
  plot.d1 = FALSE,
  plot.d2 = FALSE,
  plot.diff = TRUE,
  draw.contour = TRUE,
  contour.levels = NULL,
  xlim = extendrange(range(c(x1, x2)), f = extendrange.x),
  ylim = extendrange(range(c(y1, y2)), f = extendrange.y),
  extendrange = 0,
  extendrange.x = extendrange,
  extendrange.y = extendrange,
  col1 = "blue",
  col2 = "red",
  bg.col = "white",
  ncol = 101,
  add = FALSE,
  ...
)
}
\arguments{
\item{x1, x2, y1, y2}{the x and y values for distributions 1 and 2.}

\item{bw}{a bandwidth selection function such as \code{\link{bw.nrd}}; or a numeric vector for \code{\link{kde2d}}.}

\item{n}{the number of grid points.}

\item{ztransform}{transformation of the z axis of the 2D density. The default \code{\link{sqrt_transform}} will take square root the differences in \code{z} to increase contrast. Use \code{\link{I}} in order to not transform the axis.}

\item{zlim}{overrides the limits in the z direction, setting the value where either \code{col1} or \code{col2} is displayed in full brightness.}

\item{zlim.mult}{a multiplication factor for zlim, especially useful in \code{relative = TRUE} mode.}

\item{relative}{whether to adjust \code{zlim} to match the highest density in distribution 1 and 2 (\code{TRUE}) or to the highest difference (\code{FALSE}). Ignored if \code{zlim} is provided.}

\item{plot.d1, plot.d2}{whether to plot the 2d densities of distributions 1 and 2, respectively. Disabled by default.}

\item{plot.diff}{whether to plot the differential densities. Enabled by default.}

\item{draw.contour}{whether to overlay a \code{\link{contour}} on the diff plot. Ignored if \code{plot.diff = FALSE}.}

\item{contour.levels}{levels at which to draw contour lines, see the \code{levels} argument to \code{\link{contour}}}

\item{xlim, ylim}{limits of the plot on x and y.}

\item{extendrange, extendrange.x, extendrange.y}{extend \code{xlim} and \code{ylim} by a this fraction. Ignored if \code{xlim} and \code{ylim} are provided.}

\item{col1, col2}{colors to represent the two distributions.}

\item{bg.col}{the background color.}

\item{ncol}{the number of colors of the palette. Should be an odd number to avoid weird effects around 0}

\item{add}{if TRUE, add to the current plot instead of creating a new one.}

\item{...}{further arguments from and two other methods, in particular graphical parameters for \code{\link{image}} such as \code{main}, \code{xlab} or \code{ylab}.}
}
\value{
\code{\link[=invisible]{Invisibly}}, a \code{\link{list}} like \code{\link{kde2d}} with \code{z} the difference in densities, and the following additional \dQuote{z} elements:
\itemize{
\item{d1.z, d2.z}{Densities of distributions 1 and 2}
\item{sum.z}{The sum of the density distributions 1 and 2}
}
}
\description{
Plot the difference of densities between two distributions 1 and 2
}
\examples{
# Get some gaussian distributions
x1 <- rnorm(10000)
y1 <- rnorm(10000)
x2 <- rnorm(10000, mean = 1, sd = 0.5)
y2 <- rnorm(10000, mean = 0.5, sd = 2)
# First shot at the plot
diffdensity.plot(x1, x2, y1, y2)

# Make it smoother
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10)

# Improve contour
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
		contour.levels = c(0.01, 0.1, 0.2, 0.3, 0.4))

# Change the colors
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
		col1 = "green", col2 = "yellow", bg.col = "black")

# Also plot d1 and d2
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
plot.d1 = TRUE, plot.d2 = TRUE, relative = TRUE)

# Increase resolution
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
		n = 500)

# Try an assymetric distribution
y1 <- rexp(10000, rate = 1)
y2 <- rexp(10000, rate = 1.5)
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10)

# A bit off the density is clipped in y, so extend the limits
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
                 extendrange.y = .1)
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
                 extendrange = .1, extendrange.x = 0)
# Extend in x as well
diffdensity.plot(x1, x2, y1, y2, bw = function(x) bw.nrd(x) * 10,
                 extendrange = .1)

}
\seealso{
\href{http://stackoverflow.com/questions/28521145/r-calculate-and-plot-difference-between-two-density-countours}{The Stack Overflow answer} that inspired this function
}
