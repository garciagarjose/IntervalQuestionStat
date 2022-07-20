
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the sum of \eqn{n} interval-valued data
#'
#' @description
#' This function calculates the sum of \eqn{n} interval-valued data.
#'
#' @param x a list of interval-valued data.
#'
#' @return Returns the calculated sum of \eqn{n} interval-valued data, i.e., other interval-valued data.
#'
#' @exportMethod sum
#' @docType methods
#' @name sum
#' @family IntervalList-method
#' @rdname sum-methods
#' @aliases sum,IntervalList-method
#' 
#' @usage
#' \S4method{sum}{IntervalList}(x)
#' 
#' @references 
#' Hankin, R.K.S. (2010). A step-by-step guide to writing a simple package that uses S4 methods:
#' a "hello world" example. Technical Report. Auckland University of Technology.
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' sum(IntervalList(c(1, 3), c(2, 5)))

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

.IntervalData.sum = function(x)
{
  IntervalData(sum(sapply(x, slot, "mid")),
               sum(sapply(x, slot, "spr")),
               type = 2)
}

setMethod(f = "Summary",
          signature(x = "IntervalList"),
          definition = function(x, ..., na.rm=FALSE)
          {
            switch(.Generic,
                   sum = .IntervalData.sum(x),
                   stop(paste(.Generic, "not allowed on interval-valued data"))
            )
          }
)
