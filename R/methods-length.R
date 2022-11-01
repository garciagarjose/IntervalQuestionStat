
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Get the length of an \code{IntervalList} object
#'
#' @description
#' This function allows to get the length of an interval-valued list,
#' that is, the number of different nonempty compact real intervals
#' stored in an \code{IntervalList} object.
#'
#' @param x A list of nonempty compact real intervals
#'          stored as an \code{IntervalList} object.
#'
#' @exportMethod length
#' @docType methods
#' @name length
#' @rdname length-methods
#' @aliases length,IntervalList-method
#' 
#' @return
#' This function returns a single numeric value indicating
#' the length of a list with several nonempty compact real
#' intervals. Therefore, it always returns an \code{integer}
#' object of whose length is one.
#' 
#' @usage
#' \S4method{length}{IntervalList}(x)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Some length() examples
#' 
#' list1 <- IntervalList(c(0, 2, 5), c(1, 6, 10))
#' length(list1)
#' 
#' dataframe <- data.frame(mids = c(0.5, 4, 7.5, 5),
#'                         sprs = c(0.5, 2, 2.5, 1))
#' list2 <- IntervalList(dataframe, type = 2)
#' length(list2)

setMethod(f = "length",
          signature(x = "IntervalList"),
          definition = function(x)
          {
            length(x@mid)
          }
)
