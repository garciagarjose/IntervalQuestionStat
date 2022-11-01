
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Combine \code{IntervalList} and \code{IntervalMatrix} objects by columns
#'
#' @description
#' This function allows to combine a sequence of lists and matrices of
#' intervals, that is, \code{IntervalList} and \code{IntervalMatrix} objects, by
#' columns, and store the result as an \code{IntervalMatrix} instance.
#'
#' @param ... A sequence of lists or matrices of nonempty compact real intervals
#'            stored as \code{IntervalList} or \code{IntervalMatrix} objects,
#'            respectively.
#' @param deparse.level Currently not used (put here to match the signature of
#'                      the base implementation).
#'
#' @export
#' @name cbind
#' @rdname cbind-methods
#' @aliases cbind,IntervalListOrIntervalMatrix-method
#' 
#' @return
#' This function returns a matrix of interval-valued
#' data stored as an \code{IntervalMatrix} object.
#' 
#' @usage
#' \S4method{cbind}{IntervalListOrIntervalMatrix}(..., deparse.level = 1)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some cbind() examples
#' list1 <- IntervalList(c(0, 3), c(4, 5))
#' list2 <- IntervalList(c(3, 0), c(7, 4))
#' cbind(list1, list2)
#' 
#' matrix1 <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
#' matrix2 <- IntervalMatrix(matrix(c(1, 5, 2, 6, 0, 1, 2, 3), 2, 4))
#' cbind(matrix1, matrix2)
#' 
#' cbind(list1, matrix1)

setGeneric("cbind", signature = "...")

setMethod(f = "cbind",
          "IntervalListOrIntervalMatrix",
          definition = function(..., deparse.level = 1)
          {
            elements <- list(...)
            mids <- NULL
            sprs <- NULL
            
            for (i in seq_len(length(elements)))
            {
              mids <- cbind(mids, elements[[i]]@mid)
              sprs <- cbind(sprs, elements[[i]]@spr)
            }
            
            IntervalMatrix(cbind(mids, sprs), type = 4)
          }
)
