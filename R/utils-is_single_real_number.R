
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Check whether an object is a single real number
#'
#' @description
#' This function allows to check whether a given
#' object is a single real number or not.
#'
#' @param object An object to check whether it is a single real number.
#' 
#' @return
#' This function returns a \code{logical} value indicating whether the given
#' object is a single real number or not. Therefore, it returns \code{TRUE}
#' if its argument is is a single real number and \code{FALSE} otherwise.
#'
#' @name is_single_real_number
#' @rdname is_single_real_number
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' ## Check whether an object is a proportion 
#' is_single_real_number(0.5)         ## TRUE
#' is_single_real_number(-0.5)        ## TRUE
#' is_single_real_number(c(0.2, 0.5)) ## FALSE (not a single value)
#' is_single_real_number(TRUE)        ## FALSE (not a numeric value)
#' 
#' @noRd

is_single_real_number <- function(object)
{
  is.numeric(object) && length(object) == 1
}
