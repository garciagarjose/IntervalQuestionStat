
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Apply functions over \code{IntervalMatrix} margins
#'
#' @description
#' This function allows to apply a function over the
#' rows or columns of an interval-valued matrix.
#'
#' @param X A matrix of interval-valued data stored as
#'          an \code{IntervalMatrix} object.
#' @param MARGIN A single numeric value giving the direction which the function
#'               will be applied over. In this case, only two different options
#'               are allowed:
#'               \itemize{
#'                   \item \code{1}: The function will be applied by rows.
#'                   \item \code{2}: The function will be applied by columns.
#'               }
#' @param FUN	 The function to be applied over the selected interval-valued
#'             matrix margins.
#'
#' @exportMethod apply
#' @docType methods
#' @name apply
#' @rdname apply-methods
#' @aliases apply,IntervalMatrix-method
#' 
#' @return
#' This function returns the numeric vector or the list of interval-valued data
#' attained by applying the selected function to the specified margin (rows or
#' columns) of an interval-valued matrix. Therefore, this function always
#' returns either a \code{numeric} or either an \code{IntervalList} object,
#' respectively.
#' 
#' @usage
#' \S4method{apply}{IntervalMatrix}(X, MARGIN, FUN)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some apply() examples
#' ## IntervalMatrix definition
#' m <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
#' ## Calculate sample Aumann means by rows and columns
#' ## These code lines return IntervalList objects
#' apply(m, 1, mean)
#' apply(m, 2, mean)
#' ## Calculate sample Fréchet variance by rows and columns
#' ## These code lines return numeric vectors
#' apply(m, 1, var)
#' apply(m, 2, var)

setGeneric("apply",
           function(X, MARGIN, FUN, ..., simplify = TRUE)
             standardGeneric("apply"))

setMethod(f = "apply",
          signature(X = "IntervalMatrix"),
          definition = function(X, MARGIN, FUN)
          {
            if (! (MARGIN %in% 1:2))
              stop("'MARGIN' argument should be 1 or 2")

            output <- c()
            if (MARGIN == 1)
            {
              for (i in seq_len(nrow(X)))
              {
                output <- c(output, FUN(X[i, ]))
              }
            }

            if (MARGIN == 2)
            {
              for (i in seq_len(ncol(X)))
              {
                output <- c(output, FUN(X[, i]))
              }
            }
            
            if (all(vapply(output, class,
                           FUN.VALUE = "IntervalData") == "IntervalData"))
              output <- IntervalList(vapply(output, slot, "mid",
                                            FUN.VALUE = numeric(1)),
                                     vapply(output, slot, "spr",
                                            FUN.VALUE = numeric(1)),
                                     type = 2)
            
            return(output)
          }
)
