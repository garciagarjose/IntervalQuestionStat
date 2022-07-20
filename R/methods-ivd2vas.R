
## This file is part of the IntervalQuestionStat package

#' @title
#' Convert interval-valued data responses to visual-analogue responses
#'
#' @description
#' Reduce each interval-valued data information to its mid-point so it
#' can be considered as a response from a visual analogue scale. That is, given an interval-valued
#' data \eqn{A} this method returns its mid-point \eqn{\mathrm{mid}~A}.
#'
#' @param x an interval-valued data or a list or matrix of interval-valued data.
#'
#' @return Returns the mid-points of the given interval-valued data to be considered as visual analogue scale responses in a questionnaire.
#'
#' @usage
#' \S4method{ivd2vas}{IntervalData}(x)
#'
#' \S4method{ivd2vas}{IntervalList}(x)
#'
#' \S4method{ivd2vas}{IntervalMatrix}(x)
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @name ivd2vas
#' @rdname ivd2vas-methods
#' @docType methods
#' @family IntervalData-method
#' @family IntervalList-method
#' @family IntervalMatrix-method
#' @aliases ivd2vas,IntervalData-method
#'          ivd2vas,IntervalList-method
#'          ivd2vas,IntervalMatrix-method
#'
#' @examples
#' ## Convert to visual analogue response an interval-valued response
#' ivd2vas(IntervalData(3, 3.2))
#'
#' ## Convert to visual analogue responses a list of interval-valued responses
#' ivd2vas(IntervalList(c(3, 8.7), c(3.2, 9)))
#'
#' ## Convert to likert-type response a matrix or dataframe of interval-valued responses
#' ivd2vas(IntervalMatrix(matrix(c(1, 1.5, 3.8, 4, 2.6, 3, 6, 7), 2, 4, byrow = TRUE)))
#'
#' @export

setGeneric("ivd2vas",
           function(x) standardGeneric("ivd2vas"))

setMethod(f = "ivd2vas",
          signature(x = "IntervalData"),
          definition = function(x)
          {
            x@mid
          }
)

setMethod(f = "ivd2vas",
          signature(x = "IntervalList"),
          definition = function(x)
          {
            sapply(x, ivd2vas)
          }
)

setMethod(f = "ivd2vas",
          signature(x = "IntervalMatrix"),
          definition = function(x)
          {
            m <- matrix(nrow = nrow(x), ncol = ncol(x))
            for (i in 1:nrow(m))
            {
              for (j in 1:ncol(m))
              {
                m[i,j] <- ivd2vas(x[i,j])
              }
            }
            return(m)
          }
)
