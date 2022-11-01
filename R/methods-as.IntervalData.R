
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Convert a real number into a degenerate interval
#'
#' @description
#' This function allows to coerce a real number stored as a single
#' \code{numeric} object to a degenerate interval formed only by this real
#' number saved as an \code{IntervalData} instance.
#' 
#' @details 
#' Single real numbers could be seen as particular cases of interval-valued
#' data where each interval's lower and upper bounds are equal or,
#' alternatively, its spread is zero.
#'
#' @usage
#' \S4method{as.IntervalData}{numeric}(object)
#'
#' @param object A single real number stored as a single \code{numeric} object.
#' 
#' @return
#' This function returns a degenerate interval saved as an object
#' of class \code{IntervalData}.
#'
#' @name as.IntervalData
#' @docType methods
#' @rdname as.IntervalData
#' @aliases as.IntervalData,numeric-method
#' @exportMethod as.IntervalData
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso 
#' Other coercion function is \code{\link{as.IntervalList}()}.
#' 
#' @examples
#' ## Transform a single real-valued number into an interval
#' ## In particular, degenerate interval {1} is defined.
#' i <- as.IntervalData(1); i

setGeneric("as.IntervalData",
           function(object) standardGeneric("as.IntervalData"))

setMethod(f = "as.IntervalData",
          signature(object = "numeric"),
          definition = function(object)
          {
            if(! is_single_real_number(object))
              stop("'object'argument should be a single real number")
            
            IntervalData(object, 0, type = 2)
          }
)
