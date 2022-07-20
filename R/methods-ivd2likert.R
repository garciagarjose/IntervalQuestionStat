
## This file is part of the IntervalQuestionStat package

#' @title
#' Convert interval-valued data responses into Likert-type responses
#'
#' @description
#' Associate each interval-valued data in a questionnaire to a Likert-type response following the minimum \eqn{\theta}-distance criterion.
#'
#' @details
#' If a \eqn{k}-point Likert-type scale with reference interval \eqn{[l,u]} is considered, then the minimum distance criterion
#' consists in associating each interval-valued scale response with the real number in the set defined by \eqn{\{L_{1},L_{2},\ldots,L_{k}\},}
#' where \deqn{L_{i}=l+(i-1)\frac{u-l}{k-1},\qquad i=1,2,\ldots,k,} with the smallest \eqn{\theta}-distance to the given data. That is,
#' each interval-valued data \eqn{A} is associated with the real number \eqn{L(A)} such that
#' \deqn{L(A)=\arg\min_{L\in\{L_{1},L_{2},\ldots,L_{k}\}}d_{\theta}\left(A,\{L\}\right).}
#' If ties are produced, they are broken at random.
#'
#' @param x an interval-valued data or a list or matrix of interval-valued data.
#' @param k a single positive integer value indicating the number of different Likert-type responses to be considered. By default, \code{k}=7.
#' @param minimum a single real number indicating the lower bound of the interval-valued scale used. By default, \code{minimum}=1.
#' @param maximum a single real number indicating the upper bound of the interval-valued scale used. By default, \code{maximum}=7.
#' @param theta a single positive numeric value. By default, \code{theta}=1.
#'
#' @return Returns the nearest Likert-type responses for the given interval-valued data following the minimum \eqn{\theta}-distance criterion.
#'
#' @usage
#' \S4method{ivd2likert}{IntervalData}(x, k = 7, minimum = 1, maximum = 7, theta = 1)
#'
#' \S4method{ivd2likert}{IntervalList}(x, k = 7, minimum = 1, maximum = 7, theta = 1)
#'
#' \S4method{ivd2likert}{IntervalMatrix}(x, k = 7, minimum = 1, maximum = 7, theta = 1)
#'
#' @name ivd2likert
#' @rdname ivd2likert-methods
#' @docType methods
#' @family IntervalData-method
#' @family IntervalList-method
#' @family IntervalMatrix-method
#' @aliases ivd2likert,IntervalData-method
#'          ivd2likert,IntervalList-method
#'          ivd2likert,IntervalMatrix-method
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' ## Convert to Likert-type response an interval-valued response
#' ivd2likert(IntervalData(3, 3.2), 11, 0, 10)
#'
#' ## Convert to Likert-type responses a list of interval-valued responses
#' ivd2likert(IntervalList(c(3, 8.7), c(3.2, 9)), 11, 0, 10)
#'
#' ## Convert to Likert-type response a matrix or dataframe of interval-valued responses
#' ivd2likert(IntervalMatrix(matrix(c(1, 1.5, 3.8, 4, 2.6, 3, 6, 7), 2, 4, byrow = TRUE)))
#'
#' @export

setGeneric("ivd2likert",
           function(x, k = 7, minimum = 1, maximum = 7, theta = 1) standardGeneric("ivd2likert"))

setMethod(f = "ivd2likert",
          signature(x = "IntervalData"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            if ((((x@mid-x@spr) < minimum) && ! (all.equal(x@mid-x@spr, minimum) == TRUE)) ||
                (((x@mid+x@spr) > maximum) && ! (all.equal(x@mid+x@spr, maximum) == TRUE)))
              stop("'x' argument is out of the allowed range")

            if (theta <= 0) stop("'theta' argument must be a single positive real number")

            responses <- seq(minimum, maximum, length.out = k)
            distances <- sapply(responses,
                                function(i) distance(x, as.IntervalData(i), theta = theta))
            output <- responses[distances == min(distances)]
            if (length(output) > 1)
              output <- sample(output, 1)

            return(output)
          }
)

setMethod(f = "ivd2likert",
          signature(x = "IntervalList"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            sapply(x, function(i) ivd2likert(i, k, minimum, maximum, theta))
          }
)

setMethod(f = "ivd2likert",
          signature(x = "IntervalMatrix"),
          definition = function(x, k = 7, minimum = 1, maximum = 7, theta = 1)
          {
            m <- matrix(nrow = nrow(x), ncol = ncol(x))
            for (i in 1:nrow(m))
            {
              for (j in 1:ncol(m))
              {
                m[i,j] <- ivd2likert(x[i,j], k, minimum, maximum, theta)
              }
            }
            return(m)
          }
)
