
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Create an \code{IntervalList} object
#'
#' @description
#' For convenience, \code{IntervalList} objects or instances
#' may be created with this function.
#'
#' @param x A \code{numeric} vector, \code{matrix} or \code{data.frame}.
#' @param y \code{NULL} (default) or a \code{numeric} vector with compatible
#'          dimensions to \code{x}. 
#' @param type A single real number specifying the characterization that is
#'             being used stored as a unique \code{numeric} value. Only the
#'             following two options are allowed:
#'             \itemize{
#'                 \item \code{1}: The \emph{inf/sup}-characterization
#'                       is used (default).
#'                 \item \code{2}: The \emph{mid/spr}-characterization is used.
#'             }
#' 
#' @return 
#' This function returns the created \code{IntervalList} object.
#' 
#' @details
#' In order to create an \code{IntervalList} object, the information that
#' defines the intervals of the list (either the lower and upper bounds or
#' either the mid-points and the spreads) can be given as input to
#' \code{IntervalList()} function in two different ways. On the one hand, each
#' type of characterizing point can be stored separately in two \code{numeric}
#' vectors and then they are passed to the function through \code{x} and
#' \code{y} arguments. On the other hand, they can be stored jointly in a
#' \code{matrix} or \code{data.frame} and then only \code{x} argument is used
#' (\code{y} is left as \code{NULL} as it is defined by default).
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For other interval-valued data definition use \code{\link{IntervalData}()}
#' and \code{\link{IntervalMatrix}()} functions.
#' 
#' @export
#' 
#' @examples
#' ## The following code generates the same list of intervals in four
#' ## different ways by using different characterizations. In particular,
#' ## the following list made up of three intervals is defined,
#' ## {[0, 1], [2, 6], [5, 10] = [0.5 -+ 0.5], [4 -+ 2], [7.5 -+ 2.5]}.
#' 
#' ## First, inf/sup-characterization stored in two vectors is used.
#' list1 <- IntervalList(c(0, 2, 5), c(1, 6, 10)); list1
#' ## Then, mid/spr-characterization stored in two vectors is used.
#' list2 <- IntervalList(c(0.5, 4, 7.5), c(0.5, 2, 2.5), type = 2); list2
#' ## Then, inf/sup-characterization stored in a matrix is used.
#' matrix <- matrix(c(0, 2, 5, 1, 6, 10), 3, 2)
#' list3 <- IntervalList(matrix); list3
#' ## Finally, mid/spr-characterization stored in a data.frame is used.
#' dataframe <- data.frame(mids = c(0.5, 4, 7.5), sprs = c(0.5, 2, 2.5))
#' list4 <- IntervalList(dataframe, type = 2); list4

IntervalList <- function (x, y = NULL, type = 1)
{
  if (! type %in% 1:2) 
    stop("'type' argument should be one of 1 or 2")
  
  if (! is.null(y))
  {
    if(length(x) != length(y)) 
      stop("'x' and 'y' arguments should be of the same length")
    else {
      data <- data.frame(x, y)
    }
  }
  
  else data <- x
  
  if (! is.data.frame(data) && ! is.matrix(data) ) 
    stop("'x' should be a data.frame or a matrix")
  
  if (ncol(data) != 2) 
    stop("'x' argument should have two columns")
  
  if (type == 1)
  {
    if (any(data[, 1] > data[, 2])) 
      stop(paste("Each lower bound should be minus or equal",
                 "to its corresponding upper bound"))
    
    output <- new("IntervalList",
                  mid = (data[, 2] + data[, 1])/2,
                  spr = (data[, 2] - data[, 1])/2)
  }
  
  else if (type == 2)
  {
    if (any(data[, 2] < 0)) 
      stop("Each spread should be a positive real number")
    
    output <- new("IntervalList",
                  mid = data[, 1],
                  spr = data[, 2])
  }
}
