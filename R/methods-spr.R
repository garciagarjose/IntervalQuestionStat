
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Extract and replace the spreads of interval-valued objects
#'
#' @description
#' This functions provides customized access to the \code{spr} slot of
#' \code{IntervalData}, \code{IntervalList}, and \code{IntervalMatrix} objects,
#' so their spreads can be extracted and replaced. This does not prevent to use
#' the @ accessor, but does not force others to know the implementation details.
#'
#' @param object A single nonempty compact interval or a list or matrix
#'               with some intervals of this family, that is, an
#'               \code{IntervalData}, \code{IntervalList}, or
#'               \code{IntervalMatrix} instance.
#' @param value A \code{numeric} or \code{matrix} object with the new values
#'              of the intervals spreads.
#'               
#' @usage 
#' \S4method{spr}{IntervalData}(object)
#' 
#' \S4method{spr}{IntervalList}(object)
#' 
#' \S4method{spr}{IntervalMatrix}(object)
#' 
#' @return
#' On the one hand, \code{spr()} function returns the spreads of the intervals
#' contained in \code{IntervalData}, \code{IntervalList}, or
#' \code{IntervalMatrix} instances stored as a single \code{numeric} value, a
#' \code{numeric} vector, or a \code{matrix} object, respectively. On the other
#' hand, \code{spr<-} command does not return any value since it only allows to
#' replace the \code{spr} slot of the given interval-valued object.
#'          
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For accessing \code{mid} slot of interval-valued objects, \code{\link{mid}()}
#' function can be used. 
#' 
#' @examples
#' ## Some mid() function examples
#' 
#' ## With IntervalData
#' interval <- IntervalData(0, 1)
#' 
#' spr(interval)
#' spr(interval) <- 0.75
#' spr(interval)
#' 
#' ## With IntervalList
#' list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
#' 
#' spr(list[c(1, 3)])
#' spr(list[c(1, 3)]) <- c(1, 3)
#' spr(list[c(1, 3)])
#'
#' ## With IntervalMatrix
#' m <- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2,
#'                              7, 3, 3, 4, 4, 5), 2, 6))
#' 
#' spr(m[1, 1])
#' spr(m[1, 1]) <- 2
#' spr(m[1, 1])
#' 
#' spr(m[1, 1:2])
#' spr(m[1, 1:2]) <- c(2, 3)
#' spr(m[1, 1:2])
#' 
#' spr(m[, c(1, 3)])
#' spr(m[, c(1, 3)]) <- matrix(1:4, 2, 2)
#' spr(m[, c(1, 3)])
#' 
#' @name spr
#' @rdname spr-methods
#' @docType methods
#' @exportMethod spr
#' @aliases spr,IntervalData-method
#'          spr,IntervalList-method
#'          spr,IntervalMatrix-method
#' @export

setGeneric("spr", function(object) standardGeneric("spr"))

setMethod("spr",
          "IntervalData",
          function(object)
            object@spr
)

setMethod("spr",
          "IntervalList",
          function(object)
            object@spr
)

setMethod("spr",
          "IntervalMatrix",
          function(object)
          {
            m <- object@spr
            colnames(m) <- paste0("spr", seq_len(ncol(m)))
            rownames(m) <- seq_len(nrow(m))
            return(m)
          }
)

#' @rdname spr-methods
#' @docType methods
#' @exportMethod spr<-
#' @aliases spr<-,IntervalData-method
#'          spr<-,IntervalList-method
#'          spr<-,IntervalMatrix-method
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @usage
#' \S4method{spr}{IntervalData}(object) <- value
#' 
#' \S4method{spr}{IntervalList}(object) <- value
#' 
#' \S4method{spr}{IntervalMatrix}(object) <- value
#' 
#' @export

setGeneric("spr<-",
           function(object, value) standardGeneric("spr<-"))

setMethod(f = "spr<-",
          "IntervalData",
          definition = function(object, value)
          {
            object@spr <- value
            if (validObject(object))
              return(object)
          }
)

setMethod(f = "spr<-",
          "IntervalList",,
          definition = function(object, value)
          {
            object@spr <- value
            if (validObject(object))
              return(object)
          }
)

setMethod(f = "spr<-",
          "IntervalMatrix",
          definition = function(object, value)
          {
            object@spr <- value
            if (validObject(object))
              return(object)
          }
)
