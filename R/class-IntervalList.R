
## This file is part of the IntervalQuestionStat package for R

#' @title
#' S4 class representing a list of intervals
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{mid}:}{A vector of real numbers saved as a \code{numeric}
#'                       object specifying the mid-points of the intervals.}
#'    \item{\code{spr}:}{A vector of real numbers saved as a \code{numeric}
#'                       object specifying the spreads of the intervals.}
#' }
#'
#' @exportClass IntervalList
#' @name IntervalList-class
#' @rdname IntervalList-class
#' @docType class
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Objects of \code{IntervalList} class should be created through
#' \code{\link{IntervalList}()} function. Besides \code{IntervalList} class,
#' the \pkg{IntervalQuestionStat} package also incorporates
#' \code{\link{IntervalData-class}} and \code{\link{IntervalMatrix-class}}
#' for dealing with interval-valued data in R environment.  
#' 
#' @examples
#' showClass("IntervalList")
#' showMethods(classes = "IntervalList")

setClass("IntervalList",
         slots = c(mid = "numeric",
                   spr = "numeric")
)

setValidity("IntervalList",
            function(object)
            {
              if (any(!is.finite(c(object@mid, object@spr))))
                return(paste("Each of 'mid' and 'spr'",
                             "should be a single finite number"))
              
              if (any(object@spr < 0))
                return(paste("Each 'spr' element should",
                             "be a non-negative real number"))
              
              # OK
              return(TRUE)
            }
)
