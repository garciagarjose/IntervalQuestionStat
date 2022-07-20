
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the \eqn{\theta}-distance between two interval-valued data
#'
#' @description
#' The \eqn{\theta}-distance for two given interval-valued data \eqn{A} and \eqn{B}  was defined by Gil et al. (2002) as follows,
#' \deqn{d_{\theta}(A,B) = \sqrt{(\mathrm{mid}~A - \mathrm{mid}~B)^2+\theta\cdot(\mathrm{spr}~A -\mathrm{spr}~B)^2},}
#' where \eqn{\theta} is a positive real number.
#'
#' @param e1 an interval-valued data.
#' @param e2 an interval-valued data.
#' @param theta a single positive numeric value. By default \code{theta}=1.
#'
#' @return Returns the calculated \eqn{\theta}-distance, i.e. a single numeric value.
#'
#' @exportMethod distance
#' @docType methods
#' @name distance
#' @family IntervalData-method
#' @rdname distance-methods
#' @aliases distance,IntervalData,IntervalData-method
#'
#' @usage
#' \S4method{distance}{IntervalData,IntervalData}(e1, e2, theta = 1)

#' @references
#' Gil, M.A.; Lubiano, M.A.; Montenegro, M.; Lopez, M.T. (2002).
#' Least squares fitting of an affine function and strength of association for interval-valued data.
#' Metrika 56:97-111.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(3, 7)
#' distance(i1, i2)
#' distance(i1, i2, 1/3)

setGeneric("distance",
           function(e1, e2, theta = 1) standardGeneric("distance"))

setMethod(f = "distance",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function(e1, e2, theta = 1)
          {
            if (theta <= 0) stop("'theta' argument must be a single positive real number")

            if (is.na(e1@mid) || is.na(e1@spr) || is.na(e2@mid) || is.na(e2@spr))
              return(NA_real_)

            return(sqrt((e1@mid - e2@mid)^2 + theta * (e1@spr - e2@spr)^2))
          }
)
