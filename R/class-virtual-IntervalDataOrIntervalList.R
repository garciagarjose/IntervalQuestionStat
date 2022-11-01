
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Virtual union of \code{IntervalData} and \code{IntervalList} classes
#' 
#' @description 
#' This virtual class is defined as a superclass of
#' \code{IntervalData} and \code{IntervalList} classes.
#' 
#' @details 
#' In particular, this virtual class is defined only for internal use in the
#' \pkg{IntervalQuestionStat} package implementation since it is useful for
#' supplying several classes in some methods signatures. It is very important to
#' remark that users can not define instances of this virtual class.
#' 
#' @exportClass IntervalDataOrIntervalList
#' @name IntervalDataOrIntervalList-class
#' @rdname IntervalDataOrIntervalList-class
#' @docType class
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Besides \code{IntervalDataOrIntervalList}, other virtual class defined only
#' for internal purposes in the \pkg{IntervalQuestionStat} package is
#' \code{\link{IntervalListOrIntervalMatrix-class}}.
#' 
#' @examples
#' showClass("IntervalDataOrIntervalList")
#' showMethods(classes = "IntervalDataOrIntervalList")

setClassUnion("IntervalDataOrIntervalList", c("IntervalData", "IntervalList"))
