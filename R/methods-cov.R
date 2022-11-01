
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the sample covariance between two random intervals
#'
#' @description
#' This function calculates the sample covariance between two realizations of
#' \eqn{n} nonempty compact real intervals drawn from two random intervals
#' saved as two different \code{IntervalList} objects.
#'
#' @param x A list of intervals, that is, an \code{IntervalList} object.
#' @param y A list of intervals, that is, an \code{IntervalList} object
#'          with the same length as \code{x}.
#' @param theta A single positive real number saved as a \code{numeric} object.
#'              By default, \code{theta = 1}.
#'
#' @return
#' This function returns the calculated sample covariance of two samples
#' of \eqn{n} interval-valued data, which is defined as a real number.
#' Therefore, the output of this function is a single \code{numeric} value.
#'
#'
#' @exportMethod cov
#' @docType methods
#' @name cov
#' @rdname cov-methods
#' @aliases cov,IntervalList,IntervalList-method
#'
#' @usage
#' \S4method{cov}{IntervalList,IntervalList}(x, y, theta = 1)
#'
#' @details
#' Let \eqn{\mathcal{X}} and \eqn{\mathcal{Y}} be two interval-valued random
#' sets and let \eqn{\left((x_{1},y_{1}),(x_{2},y_{2}), \ldots,
#' (x_{n},y_{n})\right)} be a sample of \eqn{n} independent observations drawn
#' from \eqn{\left(\mathcal{X},\mathcal{Y}\right)}. Then, the sample covariance
#' between \eqn{\mathcal{X}} and \eqn{\mathcal{Y}} is defined as the following
#' real number given by
#' \deqn{s_{\mathcal{X}~\mathcal{Y}} =
#' s_{\mathrm{mid}~\mathcal{X}~\mathrm{mid}~\mathcal{Y}}+\theta\cdot
#' s_{\mathrm{spr}~\mathcal{X}~\mathrm{spr}~\mathcal{Y}},}
#' where \eqn{\theta>0} and
#' \deqn{s_{\mathrm{mid}~\mathcal{X}~\mathrm{mid}~\mathcal{Y}} =
#' \frac{1}{n}\sum_{i=1}^{n}(\mathrm{mid}~x_{i} -
#' \mathrm{mid}~\overline{x})(\mathrm{mid}~y_{i}-\mathrm{mid}~\overline{y}),}
#' \deqn{s_{\mathrm{spr}~\mathcal{X}~\mathrm{spr}~\mathcal{Y}} =
#' \frac{1}{n}\sum_{i=1}^{n}(\mathrm{spr}~x_{i} -
#' \mathrm{spr}~\overline{x})(\mathrm{spr}~y_{i}-\mathrm{spr}~\overline{y}),}
#' with \eqn{\overline{x}} and \eqn{\overline{y}} being the sample Aumann means
#' of the given one-dimensional random samples.
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Other sample central tendency and dispersion measures such as sample
#' Aumann mean and sample Fréchet variance can be calculated through
#' \code{\link{mean}()} and \code{\link{var}()} functions, respectively. 
#'
#' @examples
#' ## Some cov() examples changing theta
#' list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
#' list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 6))
#' cov(list1, list2)
#' cov(list1, list2, 1/3)
#' 
#' ## Note that cov(X, X) = var(X)
#' cov(list1, list1)
#' var(list1)
#' cov(list1, list1, 1/3)
#' var(list1, 1/3)

setGeneric("cov",
           function(x, y = NULL, theta = 1, use = "everything",
                    method = c("pearson", "kendall", "spearman"), ...)
             stats::cov(x, y, use, method))

setMethod("cov",
          signature(x = "IntervalList", y = "IntervalList"),
          function(x, y, theta = 1)
          {
            if (length(x) != length(y))
              stop("'x' and 'y' arguments should have the same length")
            
            if (! is_single_positive_real_number(theta))
              stop("'theta' argument should be a single positive real number")

            n <- length(x)
            return((n - 1) * cov(x@mid, y@mid) / n +
                     theta * (n - 1) * cov(x@spr, y@spr) / n)
          }
)
