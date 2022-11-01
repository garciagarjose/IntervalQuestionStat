
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Calculate the sum of \eqn{n} intervals
#'
#' @description
#' This function calculates the sum of \eqn{n} nonempty compact real intervals.
#' 
#' @details
#' This function generalizes the Minkowski's sum of two nonempty compact real
#' intervals explained in \link{arithmetic} section and implemented through
#' \code{+} operator's method for two \code{IntervalData} objects.
#'
#' @param x A list of intervals stored as an \code{IntervalList} object.
#'
#' @return
#' This function returns an \code{IntervalData} object with the calculated
#' sum of the given \eqn{n} intervals, which is defined
#' as another nonempty compact real interval.
#' 
#' @usage
#' \S4method{sum}{IntervalList}(x)
#'
#' @exportMethod sum
#' @docType methods
#' @name sum
#' @rdname sum-methods
#' @aliases sum,IntervalList-method
#' 
#' @references 
#' Hankin, R.K.S. (2010). A step-by-step guide to writing a simple package
#' that uses S4 methods: a "hello world" example. Technical Report.
#' Auckland University of Technology.
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For further information of the interval arithmetic see \link{arithmetic}.
#' 
#' @examples
#' ## The following code calculates the sum
#' ## of a list with two different intervals
#' list <- IntervalList(c(1, 3), c(2, 5))
#' sum(list)

setGeneric("sum",
           function(x, ..., na.rm = FALSE)
           {
             standardGeneric("sum")
           },
           useAsDefault = function(x, ..., na.rm = FALSE)
           {
             base::sum(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

.IntervalData.sum <- function(x)
{
  IntervalData(sum(x@mid),
               sum(x@spr),
               type = 2)
}

setMethod(f = "Summary",
          signature(x = "IntervalList"),
          definition = function(x, ..., na.rm = FALSE)
          {
            switch(.Generic,
                   sum = .IntervalData.sum(x),
                   stop(paste(.Generic, "not allowed on interval-valued data"))
            )
          }
)
