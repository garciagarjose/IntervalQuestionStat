
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Interval arithmetic operations
#'
#' @description
#' These commands apply basic interval-valued arithmetic operations.
#'
#' @details
#' Implementation of basic interval arithmetic calculations (see, for example,
#' Moore \emph{et al}., 2009) through Minkowski's sum (see Minkowski, 1903)
#' and a product by a scalar operation. In particular, \code{+}, \code{-},
#' and \code{*} operators allow to carry out these computations. 
#' Using \emph{mid/spr}-characterization, these operations can be settled
#' for any two interval-valued data \eqn{A} and \eqn{B} and a real number
#' \eqn{\gamma} as follows:
#' \deqn{A + B = [(\mathrm{mid}~A +\mathrm{mid}~B) \mp (\mathrm{spr}~A
#' + \mathrm{spr}~B)]} and 
#' \deqn{\gamma \cdot A =
#' \left\{ \begin{array}{ll}
#' \left[\gamma\cdot \mathrm{mid}~A\mp\gamma\cdot\mathrm{spr}~A\right] &
#' \hbox{if $\gamma \geq 0$,}\\[1.5ex]
#' \left[\gamma\cdot \mathrm{mid}~A\pm\gamma\cdot\mathrm{spr}~A\right] &
#' \hbox{if $\gamma < 0$.}\end{array}\right.}
#'
#' @param e1 A single numeric value, a vector, a matrix, a single interval, or
#'           a list or a matrix of intervals saved as a \code{numeric},
#'           \code{matrix}, \code{IntervalData}, \code{IntervalList}, or
#'           \code{IntervalMatrix} object, respectively.
#' @param e2 A single numeric value, a vector, a matrix, a single interval, or
#'           a list or a matrix of intervals saved as a \code{numeric},
#'           \code{matrix}, \code{IntervalData}, \code{IntervalList}, or
#'           \code{IntervalMatrix} object, respectively.
#'
#' @return 
#' This function returns a single interval or a list or matrix of intervals,
#' that is, an \code{IntervalData} object or an \code{IntervalList} or
#' \code{IntervalMatrix} instance, containing the result of the involved
#' operation. When these binary operators are used with \code{IntervalList} and
#' \code{IntervalMatrix} objects, they return the result of the element by
#' element operations recycling the elements of the object with the shortest
#' dimensions if necessary, showing a warning when they are recycled only
#' fractionally.
#'
#' @usage
#' \S4method{+}{numeric,IntervalData}(e1, e2)
#' \S4method{+}{IntervalData,numeric}(e1, e2)
#' \S4method{+}{IntervalData,IntervalData}(e1, e2)
#' \S4method{-}{numeric,IntervalData}(e1, e2)
#' \S4method{-}{IntervalData,numeric}(e1, e2)
#' \S4method{-}{IntervalData,IntervalData}(e1, e2)
#' \S4method{-}{IntervalData,ANY}(e1, e2) # -e1
#' \S4method{*}{numeric,IntervalData}(e1, e2)
#' \S4method{*}{IntervalData,numeric}(e1, e2)
#' 
#' \S4method{+}{numeric,IntervalList}(e1, e2)
#' \S4method{+}{IntervalList,numeric}(e1, e2)
#' \S4method{+}{IntervalData,IntervalList}(e1, e2)
#' \S4method{+}{IntervalList,IntervalData}(e1, e2)
#' \S4method{+}{IntervalList,IntervalList}(e1, e2)
#' \S4method{-}{numeric,IntervalList}(e1, e2)
#' \S4method{-}{IntervalList,numeric}(e1, e2)
#' \S4method{-}{IntervalData,IntervalList}(e1, e2)
#' \S4method{-}{IntervalList,IntervalData}(e1, e2)
#' \S4method{-}{IntervalList,IntervalList}(e1, e2)
#' \S4method{-}{IntervalList,ANY}(e1, e2) # -e1
#' \S4method{*}{numeric,IntervalList}(e1, e2)
#' \S4method{*}{IntervalList,numeric}(e1, e2)
#'
#' \S4method{+}{numeric,IntervalMatrix}(e1, e2)
#' \S4method{+}{IntervalMatrix,numeric}(e1, e2)
#' \S4method{+}{matrix,IntervalMatrix}(e1, e2)
#' \S4method{+}{IntervalMatrix,matrix}(e1, e2)
#' \S4method{+}{IntervalData,IntervalMatrix}(e1, e2)
#' \S4method{+}{IntervalMatrix,IntervalData}(e1, e2)
#' \S4method{+}{IntervalList,IntervalMatrix}(e1, e2)
#' \S4method{+}{IntervalMatrix,IntervalList}(e1, e2)
#' \S4method{+}{IntervalMatrix,IntervalMatrix}(e1, e2)
#' \S4method{-}{numeric,IntervalMatrix}(e1, e2)
#' \S4method{-}{IntervalMatrix,numeric}(e1, e2)
#' \S4method{-}{matrix,IntervalMatrix}(e1, e2)
#' \S4method{-}{IntervalMatrix,matrix}(e1, e2)
#' \S4method{-}{IntervalData,IntervalMatrix}(e1, e2)
#' \S4method{-}{IntervalMatrix,IntervalData}(e1, e2)
#' \S4method{-}{IntervalList,IntervalMatrix}(e1, e2)
#' \S4method{-}{IntervalMatrix,IntervalList}(e1, e2)
#' \S4method{-}{IntervalMatrix,IntervalMatrix}(e1, e2)
#' \S4method{-}{IntervalMatrix,ANY}(e1, e2) # -e1
#' \S4method{*}{numeric,IntervalMatrix}(e1, e2)
#' \S4method{*}{IntervalMatrix,numeric}(e1, e2)
#' \S4method{*}{matrix,IntervalMatrix}(e1, e2)
#' \S4method{*}{IntervalMatrix,matrix}(e1, e2)
#'
#' @name arithmetic
#' @rdname arithmetic-methods
#' @docType methods
#' @exportMethod +
#' @exportMethod -
#' @exportMethod *
#' @aliases +,numeric,IntervalData-method
#'          +,IntervalData,numeric-method
#'          +,IntervalData,IntervalData-method
#'          -,numeric,IntervalData-method
#'          -,IntervalData,numeric-method
#'          -,IntervalData,IntervalData-method
#'          -,IntervalData,ANY-method
#'          *,numeric,IntervalData-method
#'          *,IntervalData,numeric-method
#'          +,numeric,IntervalList-method
#'          +,IntervalList,numeric-method
#'          +,IntervalData,IntervalList-method
#'          +,IntervalList,IntervalData-method
#'          +,IntervalList,IntervalList-method
#'          -,numeric,IntervalList-method
#'          -,IntervalList,numeric-method
#'          -,IntervalData,IntervalList-method
#'          -,IntervalList,IntervalData-method
#'          -,IntervalList,IntervalList-method
#'          -,IntervalList,ANY-method
#'          *,numeric,IntervalList-method
#'          *,IntervalList,numeric-method
#'          +,numeric,IntervalMatrix-method
#'          +,IntervalMatrix,numeric-method
#'          +,matrix,IntervalMatrix-method
#'          +,IntervalMatrix,matrix-method
#'          +,IntervalData,IntervalMatrix-method
#'          +,IntervalMatrix,IntervalData-method
#'          +,IntervalList,IntervalMatrix-method
#'          +,IntervalMatrix,IntervalList-method
#'          +,IntervalMatrix,IntervalMatrix-method
#'          -,numeric,IntervalMatrix-method
#'          -,IntervalMatrix,numeric-method
#'          -,matrix,IntervalMatrix-method
#'          -,IntervalMatrix,matrix-method
#'          -,IntervalData,IntervalMatrix-method
#'          -,IntervalMatrix,IntervalData-method
#'          -,IntervalList,IntervalMatrix-method
#'          -,IntervalMatrix,IntervalList-method
#'          -,IntervalMatrix,IntervalMatrix-method
#'          -,IntervalMatrix,ANY-method
#'          *,numeric,IntervalMatrix-method
#'          *,IntervalMatrix,numeric-method
#'          *,matrix,IntervalMatrix-method
#'          *,IntervalMatrix,matrix-method
#'
#' @references
#' \itemize{
#'    \item Minkowski, H. (1903). Volumen und oberfläche.
#'    \emph{Mathematische Annalen}, 57:447-495.
#'    \item Moore, R.E.; Kearfott, R.B.; Cloud, M.J. (2009). Introduction to
#'    Interval Analysis. \emph{Society for Industrial and Applied Mathematics},
#'    USA. \doi{10.1137/1.9780898717716}.
#' }
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some basic arithmetic interval operations
#' 
#' ## IntervalData
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(2, 3)
#' 
#' i1 + i2   ## Sum of two intervals
#' i1 + 1    ## Sum of an interval and a real number
#' 1 + i1    ## Sum of a real number and an interval 
#' 
#' i1 - i2   ## Subtraction of two intervals
#' i1 - i1   ## Note that i1 - i1 is not {0}
#' i1 - 1    ## Subtraction of an interval and a real number
#' 1 - i1    ## Subtraction of a real number and an interval
#' - i1      
#' 
#' 2 * i1    ## Product between a scalar and an interval
#' -2 * i1   ## Product between a scalar and an interval
#' i1 * 2    ## Product between an interval and a scalar
#' i1 * (-2) ## Product between an interval and a scalar
#' 
#' ## IntervalList
#' list1 <- IntervalList(c(0, 3, 2, 5), c(4, 5, 4, 8))
#' list2 <- IntervalList(c(3, 0, 3, 1), c(7, 4, 6, 2))
#' 
#' list1 + list2   ## Sum of two list of intervals
#' list1 + 1       ## Sum of a list of intervals and a real number
#' 1 + list1       ## Sum of a real number and a list of intervals
#' 1:4 + list1     ## Sum of a vector and a list of intervals
#' list1 + 1:4     ## Sum of a list of intervals and a vector
#' 
#' list1 - list2   ## Subtraction of two lists of intervals
#' list1 - 1       ## Subtraction of a list of intervals and a real number
#' 1 - list1       ## Subtraction of a real number and a list of intervals
#' 1:4 - list1     ## Subtraction of a vector and a list of intervals
#' list1 - 1:4     ## Subtraction of a list of intervals and a vector
#' - list1
#' 
#' 2 * list1       ## Product between a scalar and a list of intervals
#' -2 * list1      ## Product between a scalar and a list of intervals
#' list1 * 2       ## Product between a list of intervals and a scalar
#' list1 * (-2)    ## Product between a list of intervals and a scalar
#' 1:4 * list1     ## Product between a vector and a list of intervals
#' list1 * 1:4     ## Product between a list of intervals and vector
#' 
#' ## IntervalMatrix
#' matrix1 <- IntervalMatrix(matrix(c(0, 1, 1, 2, 2, 3, 3, 4), 2, 4))
#' matrix2 <- IntervalMatrix(matrix(c(4, 5, 5, 6, 6, 7, 7, 8), 2, 4))
#' m <- matrix(1:4, 2, 2)
#' 
#' matrix1 + matrix2  ## Sum of two matrices of intervals
#' matrix1 + 1        ## Sum of a matrix of intervals and a scalar
#' 1 + matrix1        ## sum of a scalar and a matrix of intervals
#' matrix1 + m        ## Sum of a matrix of intervals and a matrix
#' m + matrix1        ## Sum of a matrix and a matrix of intervals
#' matrix1 + i1       ## Sum of a matrix of intervals and an interval
#' i1 + matrix1       ## Sum of an interval and a matrix of intervals
#' matrix1 + list1    ## Sum of a matrix and a list of intervals
#' list1 + matrix1    ## Sum of a list and a matrix of intervals
#' 
#' matrix1 - matrix2  ## Subtraction of two matrices of intervals
#' matrix1 - 1        ## Subtraction of a matrix of intervals and a scalar
#' 1 - matrix1        ## Subtraction of a scalar and a matrix of intervals
#' matrix1 - m        ## Subtraction of a matrix of intervals and a matrix
#' m - matrix1        ## Subtraction of a matrix and a matrix of intervals
#' matrix1 - i1       ## Subtraction of a matrix of intervals and an interval
#' i1 - matrix1       ## Subtraction of an interval and a matrix of intervals
#' matrix1 - list1    ## Subtraction of a matrix and a list of intervals 
#' list1 - matrix1    ## Subtraction of a list and a matrix of intervals 
#' - matrix1
#' 
#' matrix1 * 2        ## Product between a matrix of intervals and a scalar
#' 2 *  matrix1       ## Product between a scalar and a matrix of intervals
#' matrix1 * m        ## Product between a matrix of intervals and a matrix
#' m * matrix1        ## Product between a matrix and a matrix of intervals

invisible(NULL)

setMethod(f = "+",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1 + e2@mid, e2@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e2 + e1@mid, e1@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1@mid + e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1 - e2@mid, e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e1@mid - e2, e1@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1@mid - e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "ANY"),
          definition = function (e1, e2)
            IntervalData(-e1@mid, e1@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "numeric", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalData(e1 * e2@mid, abs(e1) * e2@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "IntervalData", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalData(e2 * e1@mid, abs(e2) * e1@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "numeric", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1 + e2@mid, e2@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalList", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalList(e2 + e1@mid, e1@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1@mid + e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalList", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalList(e1@mid + e2@mid, e1@spr + e2@mid, type = 2)
)

setMethod(f = "+",
          signature(e1 = "IntervalList", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1@mid + e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "numeric", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1 - e2@mid, e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalList", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalList(e1@mid - e2, e1@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1@mid - e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalList", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalList(e1@mid - e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalList", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1@mid - e2@mid, e1@spr + e2@spr, type = 2)
)

setMethod(f = "-",
          signature(e1 = "IntervalList", e2 = "ANY"),
          definition = function (e1, e2)
            IntervalList(-e1@mid, e1@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "numeric", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalList(e1 * e2@mid, abs(e1) * e2@spr, type = 2)
)

setMethod(f = "*",
          signature(e1 = "IntervalList", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalList(e2 * e1@mid, abs(e2) * e1@spr, type = 2)
)

setMethod(f = "+",
          signature(e1 = "numeric", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 + e2@mid, e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalMatrix", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e2 + e1@mid, e1@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "matrix", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 + e2@mid, e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalMatrix", e2 = "matrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e2 + e1@mid, e1@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalData", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid + e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalMatrix", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid + e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalList", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid + e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalMatrix", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid + e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "+",
          signature(e1 = "IntervalMatrix", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid + e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "numeric", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 - e2@mid, e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2, e1@spr), type = 4 )
)

setMethod(f = "-",
          signature(e1 = "matrix", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 - e2@mid, e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "matrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2, e1@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalData", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "IntervalData"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalList", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "IntervalList"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1@mid - e2@mid, e1@spr + e2@spr), type = 4)
)

setMethod(f = "-",
          signature(e1 = "IntervalMatrix", e2 = "ANY"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(-e1@mid, e1@spr), type = 4)
)

setMethod(f = "*",
          signature(e1 = "numeric", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 * e2@mid, abs(e1) * e2@spr), type = 4)
)

setMethod(f = "*",
          signature(e1 = "IntervalMatrix", e2 = "numeric"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e2 * e1@mid, abs(e2) * e1@spr), type = 4)
)

setMethod(f = "*",
          signature(e1 = "matrix", e2 = "IntervalMatrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e1 * e2@mid, abs(e1) * e2@spr), type = 4)
)

setMethod(f = "*",
          signature(e1 = "IntervalMatrix", e2 = "matrix"),
          definition = function (e1, e2)
            IntervalMatrix(cbind(e2 * e1@mid, abs(e2) * e1@spr), type = 4)
)
