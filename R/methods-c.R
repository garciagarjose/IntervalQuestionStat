
## This file is part of the IntervalQuestionStat package

#' @title
#' Combine interval-valued data into an interval-valued data list
#'
#' @description
#' This function combines its arguments into a object of class \code{IntervalList}.
#'
#' @param x an \code{IntervalData} or \code{IntervalList} object.
#' @param ... additional arguments.
#'
#' @name c
#' @rdname c-methods
#'
#' @return An interval-valued data list with the combination of given elements
#' @export
#' @aliases c,IntervalData-method
#'          c,IntervalList-method
#'
#' @usage
#' \S4method{c}{IntervalData}(x, ...)
#' \S4method{c}{IntervalList}(x, ...)
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Combine 'IntervalData' objects
#' i1 <- IntervalData(0, 1)
#' i2 <- IntervalData(0, 2)
#' list1 <- c(i1, i2)
#' list1
#'
#' ## Combine 'IntervalList' objects
#' list2 <- c(list1, list1)
#' list2
#'
#' ## Combine both 'IntervalData' and 'IntervalList' objects
#' list3 <- c(i1, list1)
#' list3
#' list4 <- c(list1, i1)
#' list4

setMethod(f = "c",
          signature = "IntervalData",
          definition = function(x, ...)
          {
            elements <- list(x, ...)
            new("IntervalList",
                unlist(sapply(elements,
                              function(object)
                              {
                                if (is(object, "IntervalList"))
                                {
                                  return(unlist(object))
                                }
                                if (is(object, "IntervalData"))
                                {
                                  return(object)
                                }
                              })))
          }
)

setMethod(f = "c",
          signature = "IntervalList",
          definition = function(x, ...)
          {
            elements <- list(x, ...)
            new("IntervalList",
                unlist(sapply(elements,
                              function(object)
                              {
                                if (is(object, "IntervalList"))
                                {
                                  return(unlist(object))
                                }
                                if (is(object, "IntervalData"))
                                {
                                  return(object)
                                }
                              })))
          }
)
