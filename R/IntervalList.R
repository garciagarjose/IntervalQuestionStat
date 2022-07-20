
## This file is part of the IntervalQuestionStat package

#' @title
#' Create an \code{IntervalList} object
#'
#' @description
#' For convenience, objects of class \code{IntervalList}
#' may be created with this function.
#'
#' @param x a numeric vector, matrix or data frame.
#' @param y NULL (default) or a numeric vector with compatible dimensions to \code{x}. 
#' @param type a number specifying the order and the characterisation that is being used. 
#' Only two options are allowed: 
#' \itemize{
#'    \item \code{1}: inf/sup-characterisation is used (default).
#'    \item \code{2}: mid/spr-characterisation is used.
#' }
#' 
#' @return Object of class \code{\linkS4class{IntervalList}}.
#'
#' @family IntervalList-method
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples 
#' IntervalList(c(0, 2, 5), c(1, 6, 10))
#' IntervalList(c(0.5, 4, 7.5), c(0.5, 2, 2.5), 2)
#' IntervalList(matrix(c(0, 2, 5, 1, 6, 10), 3, 2))
#' IntervalList(matrix(c(0.5, 4, 7.5, 0.5, 2, 2.5), 3, 2), type = 2)
#' 
#' @export

IntervalList <- function (x, y = NULL, type = 1)
{
  if (! type %in% 1:2) 
    stop("'type' argument must be one of 1 or 2")
  
  if (!is.null(y))
  {
    if(length(x) != length(y)) 
      stop("'x' and 'y' arguments must be of the same length")
    else {
      data <- data.frame(x, y)
    }
  }
  
  else data <- x
  
  if ( !is.data.frame(data) && !is.matrix(data) ) 
    stop("'x' must be a data frame or a matrix")
  if (ncol(data) != 2) 
    stop("'x' argument must have two columns")
  
  new("IntervalList",
      sapply(1:nrow(data),
             function(i) IntervalData(data[i, 1], data[i, 2], type = type)))
}
