
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Convert interval-valued responses into their equivalent numerically encoded
#' Likert-type scale answers with minimum \eqn{\theta}-distance criterion
#'
#' @description
#' This function allows to associate each nonempty compact real interval
#' collected as a response in a questionnaire designed with interval-valued
#' scales to its equivalent numerically encoded Likert-type scale answer
#' following the minimum \eqn{\theta}-distance criterion.
#'
#' @details
#' If a \eqn{k}-point Likert-type scale with reference interval \eqn{[l,u]} is
#' considered, then the minimum distance criterion consists on associating each
#' interval-valued scale response with the real number in the set defined by
#' \eqn{\{L_{1},L_{2},\ldots,L_{k}\},} where each \eqn{L_{i}} is defined as
#' follows, \deqn{L_{i}=l+(i-1)\frac{u-l}{k-1}, \qquad i=1,2,\ldots,k,}
#' with the smallest \eqn{\theta}-distance to the given data. That is,
#' each interval \eqn{A} is associated with the real number \eqn{L(A)}
#' such that \deqn{L(A)=\arg\min_{L\in\{L_{1},L_{2},\ldots, L_{k}\}} d_{\theta}
#' \left(A,\{L\}\right).} If ties are produced, they are broken at random.
#'
#' @param x Either a single interval or either a list or matrix with several
#'          intervals stored as an \code{IntervalData} object or as an
#'          \code{IntervalList} or \code{IntervalMatrix} instance.
#' @param k A single positive integer number stored as a \code{numeric} object
#'          which indicates the number of different Likert-type responses to be
#'          considered. By default, \code{k = 7}.
#' @param minimum A single real number indicating the lower bound of the
#'                interval-valued scale used saved as a unique \code{numeric}
#'                value. By default, \code{minimum = 1}.
#' @param maximum A single real number indicating the upper bound of the
#'                interval-valued scale used saved as a unique \code{numeric}
#'                value. By default, \code{maximum = 7}.
#' @param theta A single positive real number stored as a unique \code{numeric}
#'              value which is used for distance computations. By default,
#'              \code{theta = 1}.
#'
#' @return 
#' This function returns the nearest Likert-type responses for the given
#' interval-valued data following the minimum \eqn{\theta}-distance criterion
#' stored either as a \code{numeric} object if \code{x} argument is a single
#' interval or a list of intervals, that is, an \code{IntervalData} or
#' \code{IntervalList} instance, or either as a \code{data.frame} object whether
#' \code{x} is a matrix of intervals, that is, an \code{IntervalMatrix} object.
#'
#' @name ivs2likert
#' @rdname ivs2likert-methods
#' @docType methods
#' @aliases ivs2likert,IntervalData-method
#'          ivs2likert,IntervalList-method
#'          ivs2likert,IntervalMatrix-method
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Interval-valued responses can be also associated to their corresponding
#' answers in a visual analogue scale through the mid-point criterion
#' implemented in the \code{\link{ivs2vas}()} function.
#' 
#' @export
#' 
#' @examples
#' ## Some ivs2likert() examples using an interval-valued scale bounded
#' ## between 0 and 10, a 11-point Likert scale, and rho2 distance (theta = 1)
#' 
#' ## A single interval-valued response
#' i <- IntervalData(3, 3.2)
#' ivs2likert(i, k = 11, minimum = 0, maximum = 10)
#'
#' ## A list of interval-valued responses
#' list <- IntervalList(c(3, 8.7), c(3.2, 9))
#' ivs2likert(list, k = 11, minimum = 0, maximum = 10)
#'
#' ## A matrix of interval-valued responses
#' matrix <- IntervalMatrix(matrix(c(1, 2.6, 1.5, 3, 3.8, 6, 4, 7), 2, 4))
#' ivs2likert(matrix, k = 11, minimum = 0, maximum = 10)

setGeneric("ivs2likert",
           function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
             standardGeneric("ivs2likert"))

setMethod(f = "ivs2likert",
          signature(x = "IntervalData"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            if (! is_single_positive_real_number(theta))
              stop("'theta' argument should be a single positive real number")
            
            if (! is_single_positive_integer(k))
              stop("'k' argument should be a single positive integer number")
            
            if (! is_single_real_number(minimum))
              stop("'minimum' argument should be a single real number")
            
            if (! is_single_real_number(maximum))
              stop("'maximum' argument should be a single real number")
            
            if (minimum > maximum)
              stop(paste("'minimum' argument should be less",
                         "or equal than 'maximum' argument"))
            
            if ((((x@mid - x@spr) < minimum) &&
                 ! (all.equal(x@mid - x@spr, minimum) == TRUE)) ||
                (((x@mid + x@spr) > maximum) &&
                 ! (all.equal(x@mid + x@spr, maximum) == TRUE)))
              stop("'x' argument is out of the allowed range")

            responses <- seq(minimum, maximum, length.out = k)
            distances <- vapply(responses,
                                function(i) distance(x, as.IntervalData(i),
                                                     theta = theta),
                                numeric(1))
            output <- responses[distances == min(distances)]
            if (length(output) > 1)
              output <- sample(output, 1)

            return(output)
          }
)

setMethod(f = "ivs2likert",
          signature(x = "IntervalList"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            output <- c()
            for (i in seq_len(length(x)))
              output[i] <- ivs2likert(x[[i]], k, minimum, maximum, theta)
            return(output)
          }
)

setMethod(f = "ivs2likert",
          signature(x = "IntervalMatrix"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            m <- matrix(nrow = nrow(x), ncol = ncol(x))
            for (i in seq_len(nrow(m)))
            {
              for (j in seq_len(ncol(m)))
              {
                m[i, j] <- ivs2likert(x[i, j], k, minimum, maximum, theta)
              }
            }
            output <- as.data.frame(m)
            names(output) <- paste0("item", seq_len(ncol(m)))
            return(output)
          }
)
