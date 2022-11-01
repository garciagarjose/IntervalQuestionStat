
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Virtual union of \code{IntervalList} and \code{IntervalMatrix} classes
#' 
#' @description 
#' This virtual class is defined as a superclass of
#' \code{IntervalList} and \code{IntervalMatrix} classes.
#' 
#' @details 
#' In particular, this virtual class is defined only for internal use in the
#' \pkg{IntervalQuestionStat} package implementation since it is useful for
#' supplying several classes in some methods signatures. It is very important to
#' remark that users can not define instances of this virtual class.
#' 
#' @exportClass IntervalListOrIntervalMatrix
#' @name IntervalListOrIntervalMatrix-class
#' @rdname IntervalListOrIntervalMatrix-class
#' @docType class
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Besides \code{IntervalListOrIntervalMatrix}, other virtual class defined only
#' for internal purposes in the \pkg{IntervalQuestionStat} package is
#' \code{\link{IntervalDataOrIntervalList-class}}.
#' 
#' @examples
#' showClass("IntervalListOrIntervalMatrix")
#' showMethods(classes = "IntervalListOrIntervalMatrix")

setClassUnion("IntervalListOrIntervalMatrix",
              c("IntervalList", "IntervalMatrix"))
