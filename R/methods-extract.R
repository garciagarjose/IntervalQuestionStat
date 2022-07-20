
## This file is part of the IntervalQuestionStat package

#' @title
#' Extract parts of an interval-valued object
#'
#' @description
#' Extract parts of interval-valued lists or matrices.
#'
#' @param x interval-valued object from which to extract elements.
#' @param i indices specifying interval-valued list's elements or interval-valued matrix's rows to extract.
#' @param j indices specifying interval-valued matrix's columns to extract.
#'
#' @rdname extract-methods
#' @name [
#'
#' @return Selected parts of an interval-valued object
#'
#' @aliases [,IntervalList-method
#'          [,IntervalMatrix-method
#'
#' @usage
#' \S4method{[}{IntervalList}(x, i)
#' \S4method{[}{IntervalMatrix}(x, i, j)
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' list <- IntervalList(c(1,3,5), c(2, 4, 6))
#' list[1]
#' list[c(1, 3)]
#'
#' m <- IntervalMatrix(matrix(c(1, 2, 6, 7, 3, 4, 5, 6, 2, 3, 4, 5), 2, 6, byrow = TRUE))
#' m[1,1]
#' m[1,]
#' m[, 1]
#' m[, c(1,3)]
#'
#' @export

setMethod(f = "[",
          signature = "IntervalList",
          definition = function(x, i)
          {
            for (s in names(getSlots("IntervalList")))
            {
              slot(x, s) <- slot(x, s)[i]
            }
            return(x)
          }
)

setMethod("[",
          signature(x = 'IntervalMatrix', i = "ANY", j = "ANY"),
          function(x, i, j)
          {
            if (missing(i)) i <- 1:nrow(x)
            if (missing(j)) j <- 1:ncol(x)

            if (length(i) == 1 && length(j) == 1)
            {
              IntervalData(x@.Data[[i, j]]@mid,
                           x@.Data[[i, j]]@spr,
                           type = 2)
            }

            else if ((length(i) == 1 && length(j) > 1) || (length(i) > 1 && length(j) == 1))
            {
              new("IntervalList",
                  x@.Data[i, j])
            }

            else
            {
              new("IntervalMatrix",
                  x@.Data[i, j])
            }
          }
)
