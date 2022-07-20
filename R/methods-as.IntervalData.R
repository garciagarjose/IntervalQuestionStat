
## This file is part of the IntervalQuestionStat package

#' @title
#' Convert an object to \code{IntervalData} class
#'
#' @description
#' This function coerce a real number to an object of \code{IntervalData} class.
#'
#' @usage
#' \S4method{as.IntervalData}{numeric}(object)
#'
#' @param object a single real number.
#' 
#' @return Returns an object of class \code{\linkS4class{IntervalData}}.
#'
#'
#' @name as.IntervalData
#' @docType methods
#' @rdname as.IntervalData
#' @family IntervalData-method
#' @family conversion
#' @aliases as.IntervalData,numeric-method
#' @exportMethod as.IntervalData
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' as.IntervalData(1)

setGeneric("as.IntervalData",
           function(object) standardGeneric("as.IntervalData"))

setMethod(f = "as.IntervalData",
          signature(object = "numeric"),
          definition = function(object)
          {
            IntervalData(object, 0, type = 2)
          }
)
