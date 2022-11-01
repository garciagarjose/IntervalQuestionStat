
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Calculate the \eqn{\theta}-distance between two intervals
#'
#' @description
#' This function calculates the \eqn{\theta}-distance
#' between any two nonempty compact real intervals.
#'
#' @param e1 A single interval stored as an \code{IntervalData} object.
#' @param e2 A single interval stored as an \code{IntervalData} object.
#' @param theta A single positive real number stored as a unique \code{numeric}
#'              value which is used for distance computations. By default,
#'              \code{theta = 1}.
#'
#' @return 
#' This function returns the calculated \eqn{\theta}-distance between the
#' two given intervals, which is defined as a single real number. Therefore,
#' the output of this function is a single \code{numeric} value.
#' 
#' @details
#' The \eqn{\theta}-distance between any two given nonempty compact real
#' intervals, \eqn{A} and \eqn{B},  was defined by Gil \emph{et al}. (2002)
#' as the non-negative real number calculated as follows,
#' \deqn{d_{\theta}(A,B) = \sqrt{(\mathrm{mid}~A - \mathrm{mid}~B)^2 +
#' \theta\cdot(\mathrm{spr}~A -\mathrm{spr}~B)^2},}
#' where \eqn{\theta} is a positive real number.
#'
#' @exportMethod distance
#' @docType methods
#' @name distance
#' @rdname distance-methods
#' @aliases distance,IntervalData,IntervalData-method
#'
#' @usage
#' \S4method{distance}{IntervalData,IntervalData}(e1, e2, theta = 1)

#' @references
#' Gil, M.Á.; Lubiano, M.A.; Montenegro, M.; López, M.T. (2002).
#' Least squares fitting of an affine function and strength of association for
#' interval-valued data. \emph{Metrika}, 56:97-111. \doi{10.1007/s001840100160}.
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some distance() examples
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(3, 7)
#' distance(i1, i2)      ## rho2 distance
#' distance(i1, i2, 1/3) ## Bertoluzza's distance with Lebesgue measure

setGeneric("distance",
           function(e1, e2, theta = 1) standardGeneric("distance"))

setMethod(f = "distance",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function(e1, e2, theta = 1)
          {
            if (! is_single_positive_real_number(theta))
              stop("'theta' argument should be a single positive real number")

            return(sqrt((e1@mid - e2@mid)^2 + theta * (e1@spr - e2@spr)^2))
          }
)
