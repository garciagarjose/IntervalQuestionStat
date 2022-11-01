
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Create an \code{IntervalMatrix} object
#'
#' @description
#' For convenience, \code{IntervalMatrix} objects or instances
#' may be created with this function.
#'
#' @param data A \code{matrix} or \code{data.frame} with the
#'             interval-valued information.
#' @param type A single \code{numeric} value specifying both the order
#'             and the characterization that is being used for storing
#'             interval-valued information on \code{data} argument.
#'             Only the following four options are allowed:
#'             \itemize{
#'                 \item \code{1}: The \emph{inf/sup}-characterization is
#'                       used variable by variable (default).
#'                 \item \code{2}: The \emph{mid/spr}-characterization is
#'                       used variable by variable.
#'                 \item \code{3}: All the supremums follow all the
#'                       infimums in the same variable order.
#'                 \item \code{4}: All the spreads follow all the
#'                       mid-points in the same variable order.
#'             }
#' 
#' @return 
#' This function returns the created \code{IntervalMatrix} object.
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For other interval-valued data definition use \code{\link{IntervalData}()}
#' and \code{\link{IntervalList}()} functions.
#'
#' @export
#' 
#' @examples
#' ## The following code illustrates four different ways to define
#' ## the same matrix of intervals through IntervalMatrix() function.
#' ## In particular, the following 2x2 interval-valued matrix is defined:
#' ## | [0, 2] [0, 4] | = | [1 -+ 1] [2 -+ 2] |
#' ## | [1, 3] [3, 9] | = | [2 -+ 1] [6 -+ 3] |
#' 
#' ## Using inf/sup characterization variable by variable
#' data1 <- matrix(c(0, 1, 2, 3, 0, 3, 4, 9), 2, 4)
#' matrix1 <- IntervalMatrix(data1); matrix1
#' ## Using mid/spr characterization variable by variable
#' data2 <- matrix(c(1, 2, 1, 1, 2, 6, 2, 3), 2, 4)
#' matrix2 <- IntervalMatrix(data2, type = 2); matrix2
#' ## Storing all the infimums first and then all the supremums
#' data3 <- matrix(c(0, 1, 0, 3, 2, 3, 4, 9), 2, 4)
#' matrix3 <- IntervalMatrix(data3, type = 3); matrix3
#' ## Storing all the mid-points first and then all the spreads
#' data4 <- matrix(c(1, 2, 2, 6, 1, 1, 2, 3), 2, 4)
#' matrix4 <- IntervalMatrix(data4, type = 4); matrix4

IntervalMatrix <- function (data, type = 1)
{
  if (! type %in% 1:4)
    stop("'type' argument should be one of 1, 2, 3 or 4")
  
  if ( !is.data.frame(data) && !is.matrix(data) )
    stop("'x' should be a data.frame or a matrix")
  
  p <- ncol(data) 
  k <- p / 2	
  if (floor(k) != k)
    stop("Number of columns of data ( =", p, ") should be an even number")
  
  if (  (type == 1) || (type == 3) )
  {
    if (type == 1) 
    {
      infs <- data[, 2 * (0:(k - 1)) + 1, drop = FALSE] 
      sups <- data[, 2 * (1:k), drop = FALSE] 
    }
    if (type == 3) 
    {
      infs <- data[, 1:k, drop = FALSE] 
      sups <- data[, (k + 1):p, drop = FALSE]
    }
    
    new("IntervalMatrix",
        mid = unname(as.matrix((sups + infs) / 2)),
        spr = unname(as.matrix((sups - infs) / 2)))
    
  } else {
    if (type == 2)
    {
      mids <- data[, 2 * (0:(k - 1)) + 1, drop = FALSE] 
      sprs <- data[, 2 * (1:k), drop = FALSE] 
    }
    if (type == 4)   
    {
      mids <- data[, 1:k, drop = FALSE]
      sprs <- data[, (k + 1):p, drop = FALSE] 
    }
    
    new("IntervalMatrix",
        mid = unname(as.matrix(mids)),
        spr = unname(as.matrix(sprs)))
  }
}
