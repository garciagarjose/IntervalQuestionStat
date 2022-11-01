
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Convert a single interval to a list of intervals
#'
#' @description
#' This function allows to coerce a single interval saved as an
#' \code{IntervalData} object to a degenerated list of intervals composed only
#' of this interval and stored as an \code{IntervalList} instance.
#'
#'
#' @usage
#' \S4method{as.IntervalList}{IntervalData}(object)
#'
#' @param object A single interval stored as an \code{IntervalData} object.
#' 
#' @return 
#' This function returns the interval coerced to a list of intervals
#' stored as an \code{IntervalList} object.
#'
#'
#' @name as.IntervalList
#' @docType methods
#' @rdname as.IntervalList
#' @aliases as.IntervalList,IntervalData-method
#' @exportMethod as.IntervalList
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso 
#' Other coercion function is \code{\link{as.IntervalData}()}.
#' 
#' @examples
#' ## Convert a single interval into a list of intervals with a single interval
#' ## In particular, degenerate interval list {[0, 1]} = {[1 -+ 1]} is defined.
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
