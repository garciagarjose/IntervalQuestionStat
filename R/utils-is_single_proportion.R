
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Check whether an object is a single proportion
#'
#' @description
#' This function allows to check whether a
#' given object is a single proportion or not.
#'
#' @param object An object to check whether it is a single proportion.
#' 
#' @return
#' This function returns a \code{logical} value indicating whether the given
#' object is a single proportion or not. Therefore, it returns \code{TRUE} if
#' its argument is is a single proportion and \code{FALSE} otherwise.
#'
#' @name is_single_proportion
#' @rdname is_single_proportion
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' ## Check whether an object is a proportion 
#' is_single_proportion(0.5)         ## TRUE
#' is_single_proportion(c(0.2, 0.5)) ## FALSE (not a single value)
#' is_single_proportion(TRUE)        ## FALSE (not a numeric value)
#' is_single_proportion(-0.5)        ## FALSE (not in [0, 1])
#' is_single_proportion(1.5)         ## FALSE (not in [0, 1])
#' 
#' @noRd

is_single_proportion <- function(object)
{
  is.numeric(object) && length(object) == 1 && object >= 0 && object <= 1
}
