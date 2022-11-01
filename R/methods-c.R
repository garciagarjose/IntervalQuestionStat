
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Combine \code{IntervalData} and \code{IntervalList} objects
#'
#' @description
#' This function allows to combine single intervals and lists of intervals,
#' that is, \code{IntervalData} and \code{IntervalList} objects, and then
#' store the attained result as an \code{IntervalList} instance.
#'
#' @param x A single nonempty compact real interval or a unique list of
#'          of this family stored as an \code{IntervalData} object or an
#'          \code{IntervalList} instance, respectively.
#' @param ... Additional single nonempty compact real intervals or lists of
#'            intervals of this family stored as \code{IntervalData} or
#'            \code{IntervalList} instances, respectively.
#'            
#' @export
#' @name c
#' @rdname c-methods
#' @aliases c,IntervalDataOrIntervalList-method
#'
#' @return
#' This function returns the list of intervals obtained after the combination
#' of the given interval-valued elements saved as an\code{IntervalList} object.
#'
#' @usage
#' \S4method{c}{IntervalDataOrIntervalList}(x, ...)
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
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
          "IntervalDataOrIntervalList",
          definition = function(x, ...)
          {
            elements <- list(x, ...)
            mids <- c()
            sprs <- c()

            for (i in seq_len(length(elements)))
            {
              mids <- c(mids, elements[[i]]@mid)
              sprs <- c(sprs, elements[[i]]@spr)
            }

            IntervalList(x = mids, y = sprs, type = 2)
            
          }
)
