
## This file is part of the IntervalQuestionStat package

#' @title
#' Create an \code{IntervalMatrix} object
#'
#' @description
#' For convenience, objects of class \code{IntervalMatrix}
#' may be created with this function.
#'
#' @param data a matrix or data frame containing the interval-valued data information.
#' @param type a number specifying the characterisation that is being used. 
#' Only four options are allowed:
#' \itemize{
#'    \item \code{1}: inf/sup-characterisation is used variable by variable (default).
#'    \item \code{2}: mid/spr-characterisation is used variable by variable.
#'    \item \code{3}: all infimums are followed by all supremums in the same variable order.
#'    \item \code{4}: all mid-points are followed by all spreads in the same variable order.
#' }
#' 
#' @return Object of class \code{\linkS4class{IntervalMatrix}}.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' data <- matrix(c(0, 2, 0, 4, 1, 3, 3, 9), 2, 4, byrow = TRUE)
#' IntervalMatrix(data)
#' data <- matrix(c(1, 1, 2, 2, 2, 1, 6, 3), 2, 4, byrow = TRUE)
#' IntervalMatrix(data, 2)
#' data <- matrix(c(0, 0, 2, 4, 1, 3, 3, 9), 2, 4, byrow = TRUE)
#' IntervalMatrix(data, 3)
#' data <- matrix(c(1, 2, 1, 2, 2, 6, 1, 3), 2, 4, byrow = TRUE)
#' IntervalMatrix(data, 4)
#'
#' @family IntervalMatrix-method
#' @export

IntervalMatrix <- function (data, type = 1)
{
  if (! type %in% 1:4) stop("'type' argument must be one of 1, 2, 3 or 4")
  
  if ( !is.data.frame(data) && !is.matrix(data) ) stop("'x' must be a data frame or a matrix")
  
  p <- ncol(data) 
  k <- p/2	
  if (floor(k) != k) stop("Number of columns of data ( =", p, ") must be an even number")
  
  n <- nrow(data)  
  m <- matrix(list(), n, k)
  
  if (  (type == 1) || (type == 3) )
  {
    if (type == 1) 
    {
      infs <- data[, 2*(0:(k-1))+1, drop = FALSE] 
      sups <- data[, 2*(1:k), drop = FALSE] 
    }
    if (type == 3) 
    {
      infs <- data[, 1:k, drop = FALSE] 
      sups <- data[, (k+1):p, drop = FALSE]
    }
    
    for(i in 1:n)
      for (j in 1:k)
        m[[i,j]] <- IntervalData(infs[i,j], sups[i,j], type = 1)
    
    new("IntervalMatrix",
        m)
    
  } else {
    if (type == 2)
    {
      mids <- data[, 2*(0:(k-1))+1, drop = FALSE] 
      sprs <- data[, 2*(1:k), drop = FALSE] 
    }
    if (type == 4)   
    {
      mids <- data[, 1:k, drop = FALSE]
      sprs <- data[, (k+1):p, drop = FALSE] 
    }
    
    for(i in 1:n)
      for (j in 1:k)
        m[[i,j]] <- IntervalData(mids[i,j], sprs[i,j], type = 2)
    
    new("IntervalMatrix",
        m)
  }
}
