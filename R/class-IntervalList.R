
## This file is part of the IntervalQuestionStat package

#' @title
#' S4 class representing a list of interval-valued data
#' 
#' @section Slots:
#' \describe{
#'    \item{\code{.Data}:}{The data part, which is an object of class 'list'.}}
#'
#' @exportClass IntervalList
#' @name IntervalList-class
#' @rdname IntervalList-class
#' @docType class
#' @family IntervalList-method
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' showClass("IntervalList")
#' showMethods(classes = "IntervalList")

setClass("IntervalList",
         contains = "list",
         validity = function(object)
         {
           if (! all(sapply(object@.Data, class) == "IntervalData"))
             return("all elements of the list must be of class 'IntervalData'")
           
           # OK
           return(TRUE)
         })
