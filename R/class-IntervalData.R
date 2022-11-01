
## This file is part of the IntervalQuestionStat package for R

#' @title
#' S4 class representing a single interval
#'
#' @description
#' Each nonempty compact real interval \eqn{K} can be alternatively
#' characterized in terms of either its lower and upper bounds (also
#' called infimum and supremum, respectively) through what is usually
#' known as its \emph{inf/sup}-characterization or either its mid-point
#' and spread (also named center and radius, respectively) by means of its
#' \emph{mid/spr}-characterization as follows, 
#' \deqn{K = [\inf K,\sup K]=[\mathrm{mid}~K \mp \mathrm{spr}~K],}
#' where both \eqn{\inf K \leq \sup K} and \eqn{\mathrm{spr}~K \geq 0}
#' conditions are fulfilled. The existing equivalence relation between these
#' two characterizations is given by the following two equations:
#' \deqn{\mathrm{mid}~K = \frac{\sup K + \inf K}{2}
#' \qquad \mathrm{and} \qquad
#' \mathrm{spr}~K = \frac{\sup K - \inf K}{2}.}
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{mid}:}{A single real number saved as a unique \code{numeric}
#'                       value specifying the mid-point of the interval.}
#'    \item{\code{spr}:}{A single real number saved as a unique \code{numeric}
#'                       value specifying the spread of the interval.}
#' }
#'
#' @exportClass IntervalData
#' @name IntervalData-class
#' @rdname IntervalData-class
#' @docType class
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Objects of \code{IntervalData} class should be created through
#' \code{\link{IntervalData}()} function. Besides \code{IntervalData} class,
#' the \pkg{IntervalQuestionStat} package also incorporates
#' \code{\link{IntervalList-class}} and \code{\link{IntervalMatrix-class}}
#' for dealing with interval-valued data in R environment. 
#' 
#' @examples
#' showClass("IntervalData")
#' showMethods(classes = "IntervalData")

setClass(Class = "IntervalData",
         slots = c(mid = "numeric",
                   spr = "numeric")
)

setValidity("IntervalData",
            function(object)
            {
              if (length(object@mid) != 1 || length(object@spr) != 1 ||
                  any(! is.finite(c(object@mid, object@spr))))
                return(paste("Each of 'mid' and 'spr'",
                             "should be a single finite number"))
              
              if (object@spr < 0)
                return("'spr' should be a non-negative number")
              
              # OK
              return(TRUE)
            }
)
