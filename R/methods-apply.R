
## This file is part of the IntervalQuestionStat package

#' @title
#' Apply functions over \code{IntervalMatrix} margins
#'
#' @description
#' This function returns the numeric vector or the list of interval-valued data obtained
#' by applying a function to margins of an interval-valued data matrix.
#'
#' @param X a matrix of interval-valued data.
#' @param MARGIN a numeric value giving the direction which the function will be applied over.
#' \itemize{
#' \item \code{1}: the function will be applied by rows.
#' \item \code{2}: the function will be applied by columns.}
#' @param FUN	 the function to be applied.
#'
#' @exportMethod apply
#' @docType methods
#' @name apply
#' @family IntervalMatrix-method
#' @rdname apply-methods
#' @aliases apply,IntervalMatrix-method
#' 
#' @return
#' Returns the numeric vector or the list of interval-valued data obtained
#' by applying the selected function to margins of an interval-valued data matrix.
#' 
#' @usage
#' \S4method{apply}{IntervalMatrix}(X, MARGIN, FUN)
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' data <- matrix(c(0, 2, 0, 4, 1, 3, 3, 9), 2, 4, byrow = TRUE)
#' m <- IntervalMatrix(data)
#' apply(m, 1, mean)
#' apply(m, 2, mean)
#' apply(m, 1, var)
#' apply(m, 2, var)

setGeneric("apply",
           function(X, MARGIN, FUN, ..., simplify = TRUE) standardGeneric("apply"))


setMethod(f = "apply",
          signature(X = "IntervalMatrix"),
          definition = function(X, MARGIN, FUN)
          {
            if (! (MARGIN %in% 1:2)) stop("'MARGIN' argument must be 1 or 2")

            output <- c()
            if (MARGIN == 1)
            {
              for (i in 1:nrow(X))
              {
                output <- c(output, FUN(X[i,]))
              }
            }

            if (MARGIN == 2)
            {
              for (i in 1:ncol(X))
              {
                output <- c(output, FUN(X[,i]))
              }
            }

            if (all(sapply(output, class) == "IntervalData"))
              output <- new("IntervalList",
                            output)
            return(output)
          }
)
