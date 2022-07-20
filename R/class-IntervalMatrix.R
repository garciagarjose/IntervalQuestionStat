
## This file is part of the IntervalQuestionStat package

#' @title
#' S4 class representing a matrix of interval-valued data
#' 
#' @section Slots:
#' \describe{
#'    \item{\code{.Data}:}{The data part, which is an object of class 'matrix'.}}
#'
#' @exportClass IntervalMatrix
#' @name IntervalMatrix-class
#' @rdname IntervalMatrix-class
#' @docType class
#' @family IntervalMatrix-method
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' showClass("IntervalMatrix")
#' showMethods(classes = "IntervalMatrix")

setClass("IntervalMatrix",
         contains = "matrix",
         validity = function(object)
         {
           classes <-matrix(nrow = nrow(object), ncol = ncol(object))
           for(i in 1:nrow(object))
             for (j in 1:ncol(object))
               classes[i, j] <- class(object@.Data[[i, j]])
           
           if (! all(apply(classes, 1:2, function(x) x == "IntervalData")))
             return("all elements of the matrix must be of class 'IntervalData'")

           # OK
           return(TRUE)
         }
)
