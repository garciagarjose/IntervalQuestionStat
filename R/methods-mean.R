
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Calculate the sample Aumann mean of a random interval
#'
#' @description
#' This function calculates the sample Aumann mean of a single realization
#' formed by \eqn{n} nonempty compact real intervals drawn from a random
#' interval saved as an \code{IntervalList} object.
#'
#' @param x A list of intervals, that is, an \code{IntervalList} object.

#' @return 
#' This function returns an \code{IntervalData} object with the calculated
#' sample Aumann mean of the given \eqn{n} intervals, which is defined as
#' another nonempty compact real interval.
#' 
#' @details 
#' Let \eqn{\mathcal{X}} be an interval-valued random set
#' and let \eqn{\left(x_{1},x_{2},\ldots,x_{n}\right)} be a sample of \eqn{n}
#' independent observations drawn from \eqn{\mathcal{X}}. Then, the sample
#' Aumann mean (see Aumann, 1965) is defined as the following interval given by
#' \deqn{\overline{x} = \frac{1}{n}\sum_{i=1}^{n} x_{i}.}
#'
#' @exportMethod mean
#' @docType methods
#' @name mean
#' @rdname mean-methods
#' @aliases mean,IntervalList-method
#' 
#' @usage
#' \S4method{mean}{IntervalList}(x)
#' 
#' @references 
#' Aumann, R.J. (1965). Integrals of set-valued functions.
#' \emph{Journal of Mathematical Analysis and Applications}, 12(1):1-12.
#' \doi{10.1016/0022-247X(65)90049-1}.
#' 
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Other sample dispersion and covariance measures such as sample Fréchet
#' variance and sample covariance can be calculated through \code{\link{var}()}
#' and \code{\link{cov}()} functions, respectively. 
#' 
#' @examples
#' ## Some mean() trivial examples
#' list <- IntervalList(c(1, 3), c(2, 5))
#' mean(list)

setGeneric("mean",
           function(x, ...) standardGeneric("mean"))

setMethod(f = "mean",
          signature(x = "IntervalList"),
          definition = function(x)
          {
            IntervalData(mean(x@mid),
                         mean(x@spr),
                         type = 2)
          }
)
