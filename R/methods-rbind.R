

## This file is part of the IntervalQuestionStat package for R

#' @title
#' Combine \code{IntervalList} and \code{IntervalMatrix} objects by rows
#'
#' @description
#' This function allows to combine a sequence of lists and matrices of
#' intervals, that is, \code{IntervalList} and \code{IntervalMatrix} objects, by
#' rows, and store the result as an \code{IntervalMatrix} instance.
#'
#' @param ... A sequence of lists or matrices of nonempty compact real intervals
#'            stored as \code{IntervalList} or \code{IntervalMatrix} objects,
#'            respectively.
#' @param deparse.level Currently not used (put here to match the signature of
#'                      the base implementation). 
#'
#' @export
#' @name rbind
#' @rdname rbind-methods
#' @aliases rbind,IntervalListOrIntervalMatrix-method
#' 
#' @return
#' This function returns a matrix of interval-valued
#' data stored as an \code{IntervalMatrix} object.
#' 
#' @usage
#' \S4method{rbind}{IntervalListOrIntervalMatrix}(..., deparse.level = 1)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some rbind() examples
#' list1 <- IntervalList(c(0, 3), c(4, 5))
#' list2 <- IntervalList(c(3, 0), c(7, 4))
#' rbind(list1, list2)
#' 
#' matrix1 <- IntervalMatrix(matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4))
#' matrix2 <- IntervalMatrix(matrix(c(1, 5, 2, 6, 0, 1, 2, 3), 2, 4))
#' rbind(matrix1, matrix2)
#' 
#' rbind(list1, matrix1)

setGeneric("rbind", signature = "...")

setMethod(f = "rbind",
          "IntervalListOrIntervalMatrix",
          definition = function(..., deparse.level = 1)
          {
            elements <- list(...)
            mids <- NULL
            sprs <- NULL
            
            for (i in seq_len(length(elements)))
            {
              mids <- rbind(mids, elements[[i]]@mid)
              sprs <- rbind(sprs, elements[[i]]@spr)
            }
            
            IntervalMatrix(cbind(mids, sprs), type = 4)
          }
)
