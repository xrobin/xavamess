% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_range.R
\name{seq_range}
\alias{seq_range}
\title{Generate a regular sequence from a range of values}
\usage{
seq_range(range, ...)
}
\arguments{
\item{range}{a numeric vector of value}

\item{...}{arguments passed to or from methods, in particular \code{by} and \code{length.width} passed to \code{\link{seq}}.}
}
\description{
Generate a regular sequence from a range of values
}
\examples{
rangev <- rpois(5, 10)
seq_range(rangev)
# Warns with NA values
rangev.na <- c(NA, rangev)
seq_range(rangev.na)

}
