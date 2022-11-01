
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Get the number of rows of an \code{IntervalMatrix} object
#'
#' @description
#' This function allows to get the number
#' of rows of an interval-valued matrix.
#'
#' @param x A matrix of interval-valued data stored as
#'          an \code{IntervalMatrix} object.
#'
#' @exportMethod nrow
#' @docType methods
#' @name nrow
#' @rdname nrow-methods
#' @aliases nrow,IntervalMatrix-method
#' 
#' @return
#' This function returns a single numeric value indicating the number of
#' rows of an interval-valued matrix. Therefore, this function always
#' returns an \code{integer} object whose length is one.
#' 
#' @usage
#' \S4method{nrow}{IntervalMatrix}(x)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso 
#' The number of rows of an \code{IntervalMatrix} object can be obtained along
#' with the number of columns through \code{\link{dim}()} function. In an
#' analogous way, for getting the number of columns of an \code{IntervalMatrix}
#' object, \code{\link{ncol}()} function can be used.

#'
#' @examples
#' ## Some nrow() examples
#' 
#' data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
#' m1 <- IntervalMatrix(data1)
#' nrow(m1)
#' data2 <- matrix(c(1, 5, 3, 2, 6, 4, 0, 1, 3,
#'                   2, 3, 9, 4, 3, 7, 5, 6, 8), 3, 6)
#' m2 <- IntervalMatrix(data2)
#' nrow(m2)

setGeneric("nrow",
           function(x) standardGeneric("nrow"))

setMethod(f = "nrow",
          signature(x = "IntervalMatrix"),
          definition = function(x)
          {
            dim(x)[1]
          }
)
