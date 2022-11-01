
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Print basic information of interval-valued data
#'
#' @description
#' This function allows to print in the console
#' basic information of interval-valued data.
#'
#' @details
#' For \code{IntervalData} and \code{IntervalList} objects, both \emph{inf/sup}
#' and \emph{mid/spr} characterizations of the intervals are printed, and
#' for \code{IntervalMatrix} instances, the number of rows and
#' columns is shown.
#'
#' @param object A single interval, a list of intervals or a matrix with several
#'               intervals stored as an \code{IntervalData},
#'               \code{IntervalList}, or \code{IntervalMatrix} object.
#'
#' @return
#' This function does not return any value. It only prints the interval-valued
#' object's information.
#'
#' @usage
#' \S4method{show}{IntervalData}(object)
#'
#' \S4method{show}{IntervalList}(object)
#'
#' \S4method{show}{IntervalMatrix}(object)
#'
#' @exportMethod show
#' @name show
#' @aliases show,IntervalData-method
#'          show,IntervalList-method
#'          show,IntervalMatrix-method
#' @rdname show-methods
#' @docType methods
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
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
#' m <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
#' show(m)

if (!isGeneric("show"))
  setGeneric("show", function(object) standardGeneric("object"))

setMethod(f = "show",
          signature(object = "IntervalData"),
          definition = function(object)
          {
            cat(sprintf(paste0("An object of class \"IntervalData\" with:\n",
                               "    inf/sup-characterization: [%g,%g],\n",
                               "    mid/spr-characterization: [%g-+%g].\n"),
                        object@mid - object@spr, object@mid + object@spr,
                        object@mid, object@spr))
          }
)

setMethod(f = "show",
          signature(object = "IntervalList"),
          definition = function(object)
          {
            cat("An object of class \"IntervalList\"\n")
            for (i in seq_len(length(object)))
            {
              cat(paste0("[[", i, "]]\n"))
              cat(sprintf(paste0("An object of class \"IntervalData\" with:\n",
                                 "    inf/sup-characterization: [%g,%g],\n",
                                 "    mid/spr-characterization: [%g-+%g].\n\n"),
                          object@mid[i] - object@spr[i],
                          object@mid[i] + object@spr[i],
                          object@mid[i], object@spr[i]))
            }
          }
)

setMethod(f = "show",
          signature(object = "IntervalMatrix"),
          definition = function(object)
          {
            cat(sprintf(paste("An object of class \"IntervalMatrix\"",
                               "with %g rows and %g columns.\n"),
                        nrow(object), ncol(object)))
          }
)
