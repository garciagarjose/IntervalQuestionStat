
## This file is part of the IntervalQuestionStat package for R

#' @title
#' S4 class representing a matrix of intervals
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{mid}:}{A matrix of real numbers saved as a \code{matrix}
#'                       object specifying the mid-points of the intervals.}
#'    \item{\code{spr}:}{A matrix of real numbers saved as a \code{matrix}
#'                       object specifying the spreads of the intervals.}
#' }
#'
#' @exportClass IntervalMatrix
#' @name IntervalMatrix-class
#' @rdname IntervalMatrix-class
#' @docType class
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Objects of \code{IntervalMatrix} class should be created through
#' \code{\link{IntervalMatrix}()} function. Besides \code{IntervalMatrix} class,
#' the \pkg{IntervalQuestionStat} package also incorporates
#' \code{\link{IntervalData-class}} and \code{\link{IntervalList-class}}
#' for dealing with interval-valued data in R environment. 
#' 
#' @examples
#' showClass("IntervalMatrix")
#' showMethods(classes = "IntervalMatrix")

setClass("IntervalMatrix",
         slot = c(mid = "matrix",
                  spr = "matrix")
)

setValidity ("IntervalMatrix",
             function(object)
             {
               if (any(!is.finite(cbind(object@mid, object@spr))))
                 return(paste("Each of 'mid' and 'spr'",
                              "should be a single finite number"))
               
               if (any(object@spr < 0))
                 return("'spr' should be a non-negative real number")
               
               # OK
               return(TRUE)
             }

)
