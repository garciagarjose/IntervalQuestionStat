
## This file is part of the IntervalQuestionStat package

#' @title
#' Convert an object to \code{IntervalList} class
#'
#' @description
#' This function coerce an object of \code{IntervalData} to an object of \code{IntervalList} class.
#'
#'
#' @usage
#' \S4method{as.IntervalList}{IntervalData}(object)
#'
#' @param object an interval-valued data.
#' 
#' @return Returns an object of class \code{\linkS4class{IntervalList}}.
#'
#'
#' @name as.IntervalList
#' @docType methods
#' @rdname as.IntervalList
#' @family IntervalList-method
#' @family conversion
#' @aliases as.IntervalList,IntervalData-method
#' @exportMethod as.IntervalList
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' as.IntervalList(IntervalData(0, 1))

setGeneric("as.IntervalList",
           function(object) standardGeneric("as.IntervalList"))

setMethod(f = "as.IntervalList",
          signature(object = "IntervalData"),
          definition = function(object)
          {
            IntervalList(object@mid, object@spr, type = 2)
          }
)
