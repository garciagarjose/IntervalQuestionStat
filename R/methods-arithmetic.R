
## This file is part of the IntervalQuestionStat package

#' @title
#' Interval arithmetic operations
#'
#' @description
#' Applies arithmetic operations using interval-based calculations.
#'
#' @details
#' Implementation of Minkowski's sum and product by a scalar operators: \code{+}, \code{-}, \code{*}.
#' Using mid/spr-characterisation, these operations can be settled for two interval-valued
#' data \eqn{A} and \eqn{B} and a real number \eqn{\gamma} as follows:
#' \deqn{A + B = [(\mathrm{mid}~A +\mathrm{mid}~B) \mp (\mathrm{spr}~A +\mathrm{spr}~B)]} and 
#' \deqn{\gamma \cdot A =
#' \left\{ \begin{array}{ll}
#' \left[\gamma\cdot \mathrm{mid}~A\mp\gamma\cdot\mathrm{spr}~A\right] & \hbox{if $\gamma \geq 0$,}\\[1.5ex]
#' \left[\gamma\cdot \mathrm{mid}~A\pm\gamma\cdot\mathrm{spr}~A\right] & \hbox{if $\gamma < 0$.}\end{array}\right.}
#'
#' @param e1 an interval-valued data or single numeric value.
#' @param e2 an interval-valued data or single numeric value.
#'
#' @return Returns an interval-valued data of the class \code{\linkS4class{IntervalData}}.
#'
#' @usage
#' \S4method{+}{numeric,IntervalData}(e1, e2)
#'
#' \S4method{+}{IntervalData,numeric}(e1, e2)
#'
#' \S4method{+}{IntervalData,IntervalData}(e1, e2)
#'
#' \S4method{-}{numeric,IntervalData}(e1, e2)
#'
#' \S4method{-}{IntervalData,numeric}(e1, e2)
#'
#' \S4method{-}{IntervalData,IntervalData}(e1, e2)
#'
#' \S4method{-}{IntervalData,ANY}(e1, e2) # -e1
#'
#' \S4method{*}{numeric,IntervalData}(e1, e2)
#'
#' \S4method{*}{IntervalData,numeric}(e1, e2)
#'
#' @name arithmetic
#' @rdname arithmetic-methods
#' @docType methods
#' @family IntervalData-method
#' @exportMethod +
#' @exportMethod -
#' @exportMethod *
#' @aliases +,numeric,IntervalData-method
#'          +,IntervalData,numeric-method
#'          +,IntervalData,IntervalData-method
#'          -,numeric,IntervalData-method
#'          -,IntervalData,numeric-method
#'          -,IntervalData,IntervalData-method
#'          -,IntervalData,ANY-method
#'          *,numeric,IntervalData-method
#'          *,IntervalData,numeric-method
#'
#' @references
#' \itemize{
#'    \item Minkowski, H. (1903). Volumen und oberflache. Mathematische Annalen, 57:447-495.
#'    \item Moore, R.E.; Kearfott, R.B.; Cloud, M.J. (2009). Introduction to Interval Analysis. Society
#'    for Industrial and Applied Mathematics, USA.
#' }
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(2, 3)
#' -i1
#' i1 + 1
#' 1 + i1
#' i1 - 1
#' 1 - i1
#' i1 + i2
#' i1 - i2
#' 2*i1
#' i1*2
#' -2*i1
#' i1*(-2)
#'
#' ## Note that i1-i1 is not {0}
#' i1-i1

invisible(NULL)

setMethod(f = "+",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1+e2@mid, e2@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e2+e1@mid, e1@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1@mid+e2@mid, e1@spr+e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1-e2@mid, e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e1@mid-e2, e1@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1@mid-e2@mid, e1@spr+e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "ANY"),
          definition = function (e1, e2)
            IntervalData(-e1@mid, e1@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1*e2@mid, abs(e1)*e2@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e2*e1@mid, abs(e2)*e1@spr, type = 2)
)
