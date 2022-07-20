
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the sample Aumann mean of \eqn{n} interval-valued data
#'
#' @description
#' This function calculates the sample Aumann mean of \eqn{n} interval-valued data.
#'
#' @param x a list of interval-valued data.
#'
#' @return Returns the calculated sample Aumann mean of \eqn{n} interval-valued data, i.e., other interval-valued data.
#'
#'
#' @exportMethod mean
#' @docType methods
#' @name mean
#' @family IntervalList-method
#' @rdname mean-methods
#' @aliases mean,IntervalList-method
#' 
#' @usage
#' \S4method{mean}{IntervalList}(x)
#' 
#' @references 
#' Aumann, R.J. (1965). Integrals of set-valued functions. Journal of Mathematical Analysis
#' and Applications, 12(1):1-12.
#' 
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @examples
#' mean(IntervalList(c(1, 3), c(2, 5)))

setGeneric("mean",
           function(x, ...) standardGeneric("mean"))

setMethod(f = "mean",
          signature(x = "IntervalList"),
          definition = function(x)
          {
            return(IntervalData(mean(sapply(x, slot, "mid")),
                                mean(sapply(x, slot, "spr")),
                                type = 2))
          }
)
