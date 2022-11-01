
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Extract and replace parts of an interval-valued object
#'
#' @description
#' This command allows to extract and replace
#' parts of interval-valued lists or matrices.
#' 
#' @usage
#' \S4method{[}{IntervalList}(x, i)
#' 
#' \S4method{[[}{IntervalList}(x, i)
#' 
#' \S4method{[}{IntervalMatrix}(x, i, j)
#'
#' @param x A list with several nonempty compact intervals or a matrix of
#'          intervals of this family, that is, an \code{IntervalList} object or
#'          an \code{IntervalMatrix} instance.
#' @param i The indices of the elements of the list or the rows of the elements
#'          of the matrix wanted to be extracted saved as a \code{numeric},
#'          \code{integer}, or \code{logical} object.
#' @param j The indices of the columns of the matrix's elements wanted to be
#'          selected and extracted saved as a \code{numeric}, \code{integer}, or
#'          \code{logical} object.
#' @param value A single nonempty compact interval or a list or matrix of
#'              intervals saved of this family stored as an \code{IntervalData},
#'              \code{IntervalList}, or \code{IntervalMatrix} object.
#'          
#' @details
#' Both \code{i} and \code{j} can also be negative integers, indicating the
#' elements lo leave out of the selection.
#'
#' @return 
#' It should be remarked that, on the one hand, the \code{[} command returns
#' the selected elements as a list of intervals, that is, an
#' \code{IntervalList} object, when it is used for \code{IntervalList}
#' instances. On the other hand, it returns \code{IntervalData},
#' \code{IntervalList}, or \code{IntervalMatrix} objects when it is used
#' with \code{IntervalMatrix} instances. The \code{[[} command allows to
#' extract a single interval, that is, an \code{IntervalData} object, from
#' a list of several intervals saved as an \code{IntervalList} object. Finally,
#' both \code{[<-} and \code{[[<-} commands allow to replace the selected
#' elements of the given interval-valued object by another object of the
#' required class and, thus, they do not return any value.
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Extract parts of a list of intervals
#' list <- IntervalList(c(1, 3, 5), c(2, 4, 6))
#' list[1]        ## Extract the first interval
#'                ## Note that the output is an IntervalList object
#' list[[1]]      ## The first interval is extracted as an IntervalData object
#' list[c(1, 3)]  ## Extract the first and the third interval
#'
#' ## Extract parts of a matrix of intervals
#' m <- IntervalMatrix(matrix(c(1, 5, 2, 6, 6, 2, 7, 3, 3, 4, 4, 5), 2, 6))
#' m[1, 1]        ## Extract the interval from the first row and first column
#' m[1,]          ## Extract all the intervals from the first row
#' m[, 1]         ## Extract all the intervals from the first column
#' m[, c(1, 3)]   ## Extract the sub-matrix containing all the intervals
#'                ## from both first and third column of the original matrix
#'                
#' ## Replace parts of a list of intervals
#' list[[1]]
#' list[[1]] <- IntervalData(0, 1)
#' list[[1]]
#' 
#' list[c(1, 3)]
#' list[c(1, 3)] <- IntervalList(c(0, 1), c(1, 2))
#' list[c(1, 3)]
#'
#' ## Replace parts of a matrix of intervals
#' m[1, 1]
#' m[1, 1] <- IntervalData(0, 1)
#' m[1, 1]
#' 
#' m[1, 1:2]
#' m[1, 1:2] <- IntervalList(c(0, 1), c(1, 2))
#' m[1, 1:2]
#' 
#' m[, c(1, 3)]
#' m[, c(1, 3)] <- IntervalMatrix(matrix(1:8, 2, 4))
#' m[, c(1, 3)]
#' 
#' @name extract
#' @rdname extract-methods
#' @docType methods
#' @exportMethod [[
#' @exportMethod [
#' @aliases [,IntervalList-method
#'          [[,IntervalList-method
#'          [,IntervalMatrix-method
#'
#' @export

setMethod(f = "[",
          signature(x = "IntervalList"),
          definition = function(x, i)
          {
            IntervalList(x@mid[i], x@spr[i], type = 2)
          }
)

setMethod("[",
          signature(x = 'IntervalMatrix', i = "ANY", j = "ANY"),
          function(x, i, j)
          {
            if (missing(i)) i <- seq_len(nrow(x))
            if (missing(j)) j <- seq_len(ncol(x))
            
            auxi <- seq_len(nrow(x))[i]
            auxj <- seq_len(ncol(x))[j]

            if (length(auxi) == 1 && length(auxj) == 1)
            {
              IntervalData(x@mid[auxi, auxj], x@spr[auxi, auxj], type = 2)
            }

            else if ((length(auxi) == 1 && length(auxj) > 1) ||
                     (length(auxi) > 1 && length(auxj) == 1))
            {
              IntervalList(x@mid[auxi, auxj], x@spr[auxi, auxj], type = 2)
            }

            else
            {
              IntervalMatrix(cbind(x@mid[auxi, auxj], x@spr[auxi, auxj]),
                             type = 4)
            }
          }
)

setMethod(f = "[[",
          signature(x = "IntervalList"),
          definition = function(x, i)
          {
            if (! is_single_positive_integer(i))
              stop("'i' argument should be a single positive integer")
            
            IntervalData(x@mid[i], x@spr[i], type = 2)
          }
)

#' @usage
#' \S4method{[}{IntervalList}(x, i) <- value
#' 
#' \S4method{[[}{IntervalList}(x, i) <- value
#' 
#' \S4method{[}{IntervalMatrix}(x, i, j) <- value
#' @rdname extract-methods
#' @docType methods
#' @exportMethod [[<-
#' @exportMethod [<-
#' @aliases [[<-,IntervalList-method
#'          [<-,IntervalList-method
#'          [<-,IntervalMatrix-method
#'
#' @export

setMethod(f = "[<-",
          "IntervalList",
          definition = function(x, i, value)
          {
            x@mid[i] <- value@mid
            x@spr[i] <- value@spr
            
            if (validObject(x))
              return(x)
          }
)

setMethod(f = "[<-",
          "IntervalMatrix",
          definition = function(x, i, j, value)
          {
            if (missing(i)) i <- seq_len(nrow(x))
            if (missing(j)) j <- seq_len(ncol(x))
            
            if (length(i) == 1 && length(j) == 1 && is(value, "IntervalData"))
            {
              x@mid[i, j] <- value@mid
              x@spr[i, j] <- value@spr
            }
            
            else if ((length(i) == 1 && length(j) > 1) ||
                     (length(i) > 1 && length(j) == 1) &&
                     is(value, "IntervalList"))
            {
              x@mid[i, j] <- value@mid
              x@spr[i, j] <- value@spr
            }
            
            else if (length(i) > 1 && length(j) > 1 &&
                     is(value, "IntervalMatrix"))
            {
              x@mid[i, j] <- value@mid
              x@spr[i, j] <- value@spr
            }
            
            if (validObject(x))
              return(x)
          }
)

setMethod(f = "[[<-",
          signature(x = "IntervalList"),
          definition = function(x, i, value)
          {
            if (! is_single_positive_integer(i))
              stop("'i' argument should be a single positive integer")
            
            if (! is(value, "IntervalData"))
              stop("'value' argument should be an single interval")
            
            x@mid[i] <- value@mid
            x@spr[i] <- value@spr
            
            if (validObject(x))
              return(x)
          }
)
