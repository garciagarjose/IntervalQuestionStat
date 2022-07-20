
## This file is part of the IntervalQuestionStat package

#' @title
#' Create an \code{IntervalData} object
#'
#' @description
#' For convenience, objects of class \code{IntervalData}
#' may be created with this function.
#'
#' @param a1 a number specifying the infimum or the mid-point of the interval.
#' @param a2 a number specifying the supremum or the spread of the interval.
#' @param type a number specifying the characterisation that is being used. 
#' Only two options are allowed: 
#' \itemize{
#'    \item \code{1}: inf/sup-characterisation is used (default).
#'    \item \code{2}: mid/spr-characterisation is used.
#' }
#' 
#' @return Object of class \code{\linkS4class{IntervalData}}.
#'
#' @family IntervalData-method
#' 
#' @examples
#' IntervalData(0, 2)
#' IntervalData(1, 1, 2)
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @export

IntervalData <- function(a1,
                         a2,
                         type = 1)
{
  if (! type %in% 1:2) stop("'type' argument must be 1 or 2")
  
  if (type == 1)
  {
    if (a1 > a2) stop("'a1' must be minus or equal to 'a2'")
    
    output <- new("IntervalData",
                  mid = (a2+a1)/2,
                  spr = (a2-a1)/2)
  }
  
  else if (type == 2)
  {
    if (a2 < 0) stop("'a2' must be a positive real number")
    output <- new("IntervalData",
                  mid = a1,
                  spr = a2)
  }
  
  return(output)
}
