
## This file is part of the IntervalQuestionStat package

#' @title
#' Print basic information of interval-valued data
#'
#' @description
#' Print inf/sup and mid/spr characterisations of a given interval-valued data.
#'
#' @param object an object of class \code{IntervalData} or \code{IntervalMatrix}.
#'
#' @return The object's inf/sup and mid/spr characterisations.
#'
#' @usage
#' \S4method{show}{IntervalData}(object)
#'
#' \S4method{show}{IntervalMatrix}(object)
#'
#' @exportMethod show
#' @name show
#' @aliases show,IntervalData-method
#'          show,IntervalMatrix-method
#' @rdname show-methods
#' @family IntervalData-method
#' @family IntervalMatrix-method
#' @docType methods
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Show an interval-valued data
#' i <- IntervalData(0, 1)
#' show(i)
#'
#' ## Show an interval-valued data list
#' list <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
#' show(list)
#'
#' ## Show an interval-valued data matrix
#' m <- IntervalMatrix(matrix(c(0, 2, 0, 4, 1, 3, 3, 9), 2, 4, byrow = TRUE))
#' show(m)

if (!isGeneric("show"))
  setGeneric("show", function(object) standardGeneric("object"))

setMethod(f = "show",
          signature(object = "IntervalData"),
          definition = function(object)
          {
            cat(sprintf("An object of class 'IntervalData' with:\n    inf/sup-characterisation: [%g,%g],\n    mid/spr-characterisation: [%g-+%g].\n",
                        object@mid-object@spr, object@mid+object@spr, object@mid, object@spr))
          }
)

setMethod(f = "show",
          signature(object = "IntervalMatrix"),
          definition = function(object)
          {
            cat(sprintf("An object of class 'IntervalMatrix' with %g rows and %g columns.\n",
                        nrow(object), ncol(object)))
          }
)
