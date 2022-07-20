
## This file is part of the IntervalQuestionStat package

#' @title
#' Calculate the sample Frechet variance of \eqn{n} interval-valued data
#'
#' @description
#' This function calculates the sample Frechet variance of \eqn{n} interval-valued data.
#'
#' @param x a list of interval-valued data.
#' @param theta a single positive numeric value. By default, \code{theta}=1.
#'
#' @return Returns the calculated sample Frechet variance of \eqn{n} interval-valued data, i.e., a real number.
#'
#' @details
#' Let \eqn{\mathcal{X}} be an interval-valued random set
#' and let \eqn{\left(x_{1},x_{2},\ldots,x_{n}\right)} be a sample of \eqn{n}
#' independent observations drawn from \eqn{\mathcal{X}}.Then, the sample Frechet variance is defined as follows,
#' \deqn{s_{\mathcal{X}}^{2}=\frac{1}{n}\sum_{i=1}^{n}d_{\theta}^{2}\left(x_{i}, \overline{x}\right)} where \eqn{\theta>0} and
#' \eqn{\overline{x}} denotes the sample Aumann mean of \eqn{\left(x_{1},x_{2},\ldots,x_{n}\right)}.
#'
#' @exportMethod var
#' @docType methods
#' @name var
#' @family IntervalList-method
#' @rdname var-methods
#' @aliases var,IntervalList-method
#'
#' @usage
#' \S4method{var}{IntervalList}(x, theta = 1)
#'
#' @references
#' Frechet, M. (1948). Les elements aleatoires de nature quelconque dans un espace distancie.
#' Annales de l'institut Henri Poincare, 10(4):215-310.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#'
#' @examples
#' var(IntervalList(c(1, 3), c(2, 5)))
#' var(IntervalList(c(1, 3), c(2, 5)), 1/3)

setGeneric("var",
           function(x, theta = 1, y = NULL, na.rm = FALSE, use, ...) stats::var(x, y, na.rm, use))

setMethod("var",
          signature(x = "IntervalList"),
          function(x, theta = 1)
          {
            if (theta <= 0) stop("'theta' argument must be a single positive real number")

            n <- length(x)
            return((n-1)*var(sapply(x, slot, "mid"))/n + theta*(n-1)*var(sapply(x, slot, "spr"))/n)
          }
)
