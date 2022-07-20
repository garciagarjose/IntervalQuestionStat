
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the covariance of two samples of \eqn{n} interval-valued data
#'
#' @description
#' This function calculates the covariance of two samples of \eqn{n} interval-valued data.
#'
#' @param x a list of interval-valued data.
#' @param y a list of interval-valued data with the same length as \code{x}.
#' @param theta a single positive numeric value. By default, \code{theta}=1.
#'
#' @return Returns the calculated covariance of two samples of \eqn{n} interval-valued data, i.e., a real number.
#'
#'
#' @exportMethod cov
#' @docType methods
#' @name cov
#' @family IntervalList-method
#' @rdname cov-methods
#' @aliases cov,IntervalList,IntervalList-method
#'
#' @usage
#' \S4method{cov}{IntervalList,IntervalList}(x, y, theta = 1)
#'
#' @details
#' Let \eqn{\mathcal{X}} and \eqn{\mathcal{Y}} be two interval-valued random sets
#' and let \eqn{\left((x_{1},y_{1}),(x_{2},y_{2}),\ldots,(x_{n},y_{n})\right)}
#' be a sample of \eqn{n} independent observations drawn from \eqn{\left(\mathcal{X},\mathcal{Y}\right)}.
#' Then, the sample covariance is defined as follows,
#' \deqn{s_{\mathcal{X}\mathcal{Y}}=s_{\mathrm{mid}~\mathcal{X}\mathrm{mid}~\mathcal{Y}}+\theta\cdot s_{\mathrm{spr}~\mathcal{X}\mathrm{spr}~\mathcal{Y}},}
#' where \eqn{\theta>0} and \deqn{s_{\mathrm{mid}~\mathcal{X}\mathrm{mid}~\mathcal{Y}}=\frac{1}{n}\sum_{i=1}^{n}(\mathrm{mid}~x_{i}-\mathrm{mid}~\overline{x})(\mathrm~{mid}~y_{i}-\mathrm{mid}~\overline{y}),}
#' with \eqn{\overline{x}} and \eqn{\overline{y}} being the sample Aumann means of the given random samples.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' list <- IntervalList(c(1, 3), c(2, 5))
#' cov(list, list)
#' var(list)
#' cov(list, list, 1/3)
#' var(list, 1/3)
#'
#' list1 <- IntervalList(c(0, 3, 2, 5, 6), c(4, 5, 4, 8, 7))
#' list2 <- IntervalList(c(3, 0, 3, 1, 4), c(7, 4, 6, 2, 6))
#' cov(list1, list2)
#' cov(list1, list2, 1/3)

setGeneric("cov",
           function(x, y = NULL, theta = 1, use = "everything", method = c("pearson", "kendall", "spearman"), ...) stats::cov(x, y, use, method))

setMethod("cov",
          signature(x = "IntervalList", y = "IntervalList"),
          function(x, y, theta = 1)
          {
            if (length(x) != length(y)) stop("'x' and 'y' arguments must have the same length")
            if (theta <= 0) stop("'theta' argument must be a single positive real number")

            n <- length(x)
            return((n-1)*cov(sapply(x, slot, "mid"), sapply(y, slot, "mid"))/n + theta*(n-1)*cov(sapply(x, slot, "spr"), sapply(y, slot, "spr"))/n)
          }
)
