
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Extract and replace the mid-points of interval-valued objects
#'
#' @description
#' This functions provides customized access to the \code{mid} slot of
#' \code{IntervalData}, \code{IntervalList}, and \code{IntervalMatrix} objects,
#' so the mid-points of the intervals can be extracted and replaced. This does
#' not prevent to use the @ accessor, but does not force others to know the
#' implementation details.
#'
#' @param object A single nonempty compact interval or a list or matrix
#'               with some intervals of this family, that is, an
#'               \code{IntervalData}, \code{IntervalList}, or
#'               \code{IntervalMatrix} instance.
#' @param value A \code{numeric} or \code{matrix} object with the new values
#'              of the intervals mid-points.
#'               
#' @usage 
#' \S4method{mid}{IntervalData}(object)
#' 
#' \S4method{mid}{IntervalList}(object)
#' 
#' \S4method{mid}{IntervalMatrix}(object)
#' 
#' @return
#' On the one hand, \code{mid()} function returns the mid-points of the
#' intervals contained in \code{IntervalData}, \code{IntervalList}, or
#' \code{IntervalMatrix} instances stored as a single \code{numeric} value, a
#' \code{numeric} vector, or a \code{matrix} object, respectively. On the other
#' hand, \code{mid<-} command does not return any value since it only allows to
#' replace the \code{mid} slot of the given interval-valued object.
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' For accessing \code{spr} slot of interval-valued objects, \code{\link{spr}()}
#' function can be used. 
#' 
#' @examples
#' ## Some mid() function examples
#' 
#' ## With IntervalData objects
#' interval <- IntervalData(0, 1)
#' 
#' mid(interval)
#' mid(interval) <- 0.75
#' mid(interval)
#' 
#' ## With IntervalList objects
#' list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
#' 
#' mid(list[c(1, 3)])
#' mid(list[c(1, 3)]) <- c(1, 3)
#' mid(list[c(1, 3)])
#'
#' ## With IntervalMatrix objects
#' m <- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2,
#'                              7, 3, 3, 4, 4, 5), 2, 6))
#' 
#' mid(m[1, 1])
#' mid(m[1, 1]) <- 2
#' mid(m[1, 1])
#' 
#' mid(m[1, 1:2])
#' mid(m[1, 1:2]) <- c(2, 3)
#' mid(m[1, 1:2])
#' 
#' mid(m[, c(1, 3)])
#' mid(m[, c(1, 3)]) <- matrix(1:4, 2, 2)
#' mid(m[, c(1, 3)])
#' 
#' @name mid
#' @rdname mid-methods
#' @docType methods
#' @exportMethod mid
#' @aliases mid,IntervalData-method
#'          mid,IntervalList-method
#'          mid,IntervalMatrix-method
#'          
#' @export

setGeneric("mid", function(object) standardGeneric("mid"))

setMethod("mid",
          "IntervalData",
          function(object)
            object@mid
)

setMethod("mid",
          "IntervalList",
          function(object)
            object@mid
)

setMethod("mid",
          "IntervalMatrix",
          function(object)
          {
            m <- object@mid
            colnames(m) <- paste0("mid", seq_len(ncol(m)))
            rownames(m) <- seq_len(nrow(m))
            return(m)
          }
)

#' @usage
#' \S4method{mid}{IntervalData}(object) <- value
#' 
#' \S4method{mid}{IntervalList}(object) <- value
#' 
#' \S4method{mid}{IntervalMatrix}(object) <- value
#' 
#' @rdname mid-methods
#' @docType methods
#' @exportMethod mid<-
#' @aliases mid<-,IntervalData-method
#'          mid<-,IntervalList-method
#'          mid<-,IntervalMatrix-method
#' 
#' @export

setGeneric("mid<-",
           function(object, value) standardGeneric("mid<-"))

setMethod(f = "mid<-",
          "IntervalData",
          definition = function(object, value)
          {
            object@mid <- value
            if (validObject(object))
              return(object)
          }
)

setMethod(f = "mid<-",
          "IntervalList",,
          definition = function(object, value)
          {
            object@mid <- value
            if (validObject(object))
              return(object)
          }
)

setMethod(f = "mid<-",
          "IntervalMatrix",
          definition = function(object, value)
          {
            object@mid <- value
            if (validObject(object))
              return(object)
          }
)
