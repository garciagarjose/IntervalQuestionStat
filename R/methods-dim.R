
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Get the number of rows and columns of an \code{IntervalMatrix} object
#'
#' @description
#' This function allows to get the number of rows
#' and columns of an interval-valued matrix.
#'
#' @param x A matrix of interval-valued data stored as
#'          an \code{IntervalMatrix} object.
#'
#' @exportMethod dim
#' @docType methods
#' @name dim
#' @rdname dim-methods
#' @aliases dim,IntervalMatrix-method
#'
#' @return
#' This function returns a bidimensional vector with the number of rows
#' and columns, respectively, of an interval-valued matrix. Therefore,
#' it always returns an \code{integer} object whose length is two.
#'
#' @usage
#' \S4method{dim}{IntervalMatrix}(x)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' The \pkg{IntervalQuestionStat} package also allows to obtain the number of
#' rows and columns of an \code{IntervalMatrix} object separately through
#' \code{\link{nrow}()} and \code{\link{ncol}()} functions, respectively.
#'
#' @examples
#' ## Some dim() examples
#'
#' data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
#' m1 <- IntervalMatrix(data1)
#' dim(m1)
#'
#' data2 <- matrix(c(1, 5, 3, 2, 6, 4, 0, 1, 3,
#'                   2, 3, 9, 4, 3, 7, 5, 6, 8), 3, 6)
#' m2 <- IntervalMatrix(data2)
#' dim(m2)

setMethod(f = "dim",
          signature(x = "IntervalMatrix"),
          definition = function(x)
          {
            dim(x@mid)
          }
)
