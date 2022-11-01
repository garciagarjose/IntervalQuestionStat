
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Create an \code{IntervalData} object
#'
#' @description
#' For convenience, \code{IntervalData} objects or instances
#' may be created with this function.
#'
#' @param a1 A single real number specifying either the infimum or either
#'           the mid-point of the interval stored as a unique \code{numeric}
#'           value.
#' @param a2 A single real number specifying either the supremum or either
#'           the spread of the interval stored as a unique \code{numeric}
#'           value.
#' @param type A single real number specifying the characterization that
#'             is being used stored as a unique \code{numeric} value. 
#'             Only two options are allowed: 
#'             \itemize{
#'                 \item \code{1}: The \emph{inf/sup}-characterization
#'                       is used (default).
#'                 \item \code{2}: The \emph{mid/spr}-characterization is used.
#'             }
#' 
#' @return
#' This function returns the created \code{IntervalData} object.
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For other interval-valued data definition use \code{\link{IntervalList}()}
#' and \code{\link{IntervalMatrix}()} functions.
#' 
#' @export
#' 
#' @examples
#' ## The following code generates the same interval through
#' ## both inf/sup and mid/spr characterizations, respectively.
#' ## In particular, interval [0, 2] = [1 -+ 1] is defined.
#' i1 <- IntervalData(a1 = 0, a2 = 2, type = 1); i1
#' i2 <- IntervalData(a1 = 1, a2 = 1, type = 2); i2

IntervalData <- function(a1,
                         a2,
                         type = 1)
{
  if (! is_single_real_number(a1))
    stop("'a1' argument should be a single real number")
  
  if (! is_single_real_number(a2))
    stop("'a2' argument should be a single real number")
  
  if (! type %in% 1:2) 
    stop("'type' argument should be 1 or 2")
  
  if (type == 1)
  {
    if (a1 > a2) 
      stop("'a1' should be minus or equal to 'a2'")
    
    output <- new("IntervalData",
                  mid = (a2 + a1) / 2,
                  spr = (a2 - a1) / 2)
  }
  
  else if (type == 2)
  {
    if (a2 < 0) 
      stop("'a2' should be a positive real number")
    
    output <- new("IntervalData",
                  mid = a1,
                  spr = a2)
  }
  
  return(output)
}
