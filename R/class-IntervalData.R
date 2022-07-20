
## This file is part of the IntervalQuestionStat package

#' @title
#' S4 class representing an interval-valued data
#'
#' @description
#' Each nonempty compact real interval \eqn{K} can be characterised in terms of either
#' its infimum and supremum or its mid-point and spread as follows, 
#' \deqn{K = [\inf K,\sup K]=[\mathrm{mid}~K \mp \mathrm{spr}~K],}
#' where \eqn{\inf K \leq \sup K} and \eqn{\mathrm{spr}~K \geq 0.}
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{mid}:}{Single numeric value specifying the mid-point of the interval.}
#'    \item{\code{spr}:}{Single numeric value specifying the spread of the interval.}}
#'
#' @exportClass IntervalData
#' @name IntervalData-class
#' @rdname IntervalData-class
#' @docType class
#' @family IntervalData-method
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' showClass("IntervalData")
#' showMethods(classes = "IntervalData")

setClass(Class = "IntervalData",
         representation(mid = "numeric",
                        spr = "numeric"),
         prototype = prototype(mid = NA_real_,
                               spr = NA_real_),
         validity = function(object)
         {
           if (length(object@mid) != 1 || length(object@spr) != 1 ||
               any(!is.finite(c(object@mid, object@spr))))
             return("Each of 'mid' and 'spr' should be a single finite real number")
           
           if (object@spr < 0)
             return("Please provide spr >= 0")
           
           # OK
           return(TRUE)
         }
)
