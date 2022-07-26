% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-ivs2vas.R
\docType{methods}
\name{ivs2vas}
\alias{ivs2vas}
\alias{ivs2vas,IntervalData-method}
\alias{ivs2vas,IntervalList-method}
\alias{ivs2vas,IntervalMatrix-method}
\title{Convert interval-valued responses into equivalent visual
analogue scale answers through mid-point criterion}
\usage{
ivs2vas(x)
}
\arguments{
\item{x}{Either a single interval or either a list or matrix with several
intervals stored as an \code{IntervalData} object or as an
\code{IntervalList} or \code{IntervalMatrix} instance.}
}
\value{
This function returns the mid-points of the given interval-valued data to be
considered as visual analogue scale responses in a questionnaire stored
either as a \code{numeric} object if \code{x} argument is a single
interval or a list of intervals, that is, an \code{IntervalData} or
\code{IntervalList} instance, or either as a \code{data.frame} object whether
\code{x} is a matrix of intervals, that is, an \code{IntervalMatrix} object.
}
\description{
This function allows to reduce each nonempty compact real interval collected
as a response in a questionnaire designed with interval-valued scales to its
mid-point so it can be considered as an answer from a visual analogue scale.
That is, given an nonempty compact real, interval, \eqn{A}, this method
returns its mid-point, which can be calculated as follows,
\deqn{\mathrm{mid}~A=\frac{\sup + \inf A}{2}.}
}
\examples{
## Some ivs2vas() examples

## A single interval-valued response
i <- IntervalData(3, 3.2)
ivs2vas(i)

## A list of interval-valued responses
list <- IntervalList(c(3, 8.7), c(3.2, 9))
ivs2vas(list)

## A matrix of interval-valued responses
matrix <- IntervalMatrix(matrix(c(1, 2.6, 1.5, 3, 3.8, 6, 4, 7), 2, 4))
ivs2vas(matrix)
}
\seealso{
Interval-valued responses can be also associated to their corresponding
Likert-type scale's answer through the minimum \eqn{\theta}-distance
criterion implemented in the \code{\link{ivs2likert}()} function.
}
\author{
José García-García \email{garciagarjose@uniovi.es}
}
