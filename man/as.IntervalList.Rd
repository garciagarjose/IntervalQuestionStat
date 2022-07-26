% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-as.IntervalList.R
\docType{methods}
\name{as.IntervalList}
\alias{as.IntervalList}
\alias{as.IntervalList,IntervalData-method}
\title{Convert a single interval to a list of intervals}
\usage{
\S4method{as.IntervalList}{IntervalData}(object)
}
\arguments{
\item{object}{A single interval stored as an \code{IntervalData} object.}
}
\value{
This function returns the interval coerced to a list of intervals
stored as an \code{IntervalList} object.
}
\description{
This function allows to coerce a single interval saved as an
\code{IntervalData} object to a degenerated list of intervals composed only
of this interval and stored as an \code{IntervalList} instance.
}
\examples{
## Convert a single interval into a list of intervals with a single interval
## In particular, degenerate interval list {[0, 1]} = {[1 -+ 1]} is defined.
as.IntervalList(IntervalData(0, 1))
}
\seealso{
Other coercion function is \code{\link{as.IntervalData}()}.
}
\author{
José García-García \email{garciagarjose@uniovi.es}
}
