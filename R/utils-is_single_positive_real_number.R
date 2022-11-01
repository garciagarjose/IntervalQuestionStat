
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Check whether an object is a single positive real number 
#'
#' @description
#' This function allows to check whether a given
#' object is a single positive real number or not.
#'
#' @param object An object to check whether it is a single positive real number.
#' 
#' @return
#' This function returns a \code{logical} value indicating whether the given
#' object is a single positive real number or not. Therefore, it returns
#' \code{TRUE} if its argument is is a single positive real number and
#' \code{FALSE} otherwise.
#'
#' @name is_single_positive_real_number
#' @rdname is_single_positive_real_number
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' ## Check whether an object is a single positive real number 
#' is_single_positive_real_number(2.5)         ## TRUE
#' is_single_positive_real_number(c(2.5, 4.5)) ## FALSE (not a single value)
#' is_single_positive_real_number(TRUE)        ## FALSE (not a numeric value)
#' is_single_positive_real_number(-2.5)        ## FALSE (not a positive value)
#' 
#' @noRd

is_single_positive_real_number <- function(object)
{
  is.numeric(object) && length(object) == 1 && object > 0 
}
