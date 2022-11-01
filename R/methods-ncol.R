
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Get the number of columns of an \code{IntervalMatrix} object
#'
#' @description
#' This function allows to get the number
#' of columns of an interval-valued matrix.
#'
#' @param x A matrix of interval-valued data stored as
#'          an \code{IntervalMatrix} object.
#'
#' @exportMethod ncol
#' @docType methods
#' @name ncol
#' @family IntervalMatrix-method
#' @rdname ncol-methods
#' @aliases ncol,IntervalMatrix-method
#'
#' @return
#' This function returns a single numeric value indicating the number of
#' columns of an interval-valued matrix. Therefore, this function always
#' returns an \code{integer} object whose length is one.
#'
#' @usage
#' \S4method{ncol}{IntervalMatrix}(x)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso 
#' The number of columns of an \code{IntervalMatrix} object can be obtained
#' along with the number of rows through \code{\link{dim}()} function. In an
#' analogous way, for getting the number of rows of an
#' \code{IntervalMatrix} object, \code{\link{nrow}()} function can be used.
#'
#' @examples
#' ## Some ncol() examples
#' 
#' data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
#' m1 <- IntervalMatrix(data1)
#' ncol(m1)
#' data2 <- matrix(c(1, 5, 3, 2, 6, 4, 0, 1, 3,
#'                   2, 3, 9, 4, 3, 7, 5, 6, 8), 3, 6)
#' m2 <- IntervalMatrix(data2)
#' ncol(m2)

setGeneric("ncol",
           function(x) standardGeneric("ncol"))

setMethod(f = "ncol",
          signature(x = "IntervalMatrix"),
          definition = function(x)
          {
            dim(x)[2]
          }
)
