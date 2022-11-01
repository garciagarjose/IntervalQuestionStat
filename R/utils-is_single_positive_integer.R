
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Check whether an object is a single positive integer 
#'
#' @description
#' This function allows to check whether a given
#' object is a single positive integer or not.
#'
#' @param object An object to check whether it is a single positive integer.
#' 
#' @return
#' This function returns a \code{logical} value indicating whether the given
#' object is a single positive integer or not. Therefore, it returns \code{TRUE}
#' if its argument is is a single positive integer and \code{FALSE} otherwise.
#'
#' @name is_single_positive_integer
#' @rdname is_single_positive_integer
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' ## Check whether an object is a single positive integer 
#' is_single_positive_integer(2)       ## TRUE
#' is_single_positive_integer(c(2, 4)) ## FALSE (not a single value)
#' is_single_positive_integer(TRUE)    ## FALSE (not a numeric value)
#' is_single_positive_integer(-2)      ## FALSE (not a positive value)
#' is_single_positive_integer(2.5)     ## FALSE (not an integer value)
#' 
#' @noRd

is_single_positive_integer <- function(object)
{
  is.numeric(object) && length(object) == 1 &&
    object > 0 && (object - floor(object) == 0)
}
