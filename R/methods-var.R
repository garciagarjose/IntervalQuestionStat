
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Calculate the sample Fréchet variance of a random interval
#'
#' @description
#' This function calculates the sample Fréchet variance of a single realization
#' of \eqn{n} nonempty compact real intervals drawn from an interval-valued
#' random set stored as an \code{IntervalList} object.
#'
#' @param x A list of intervals, that is, an \code{IntervalList} object.
#' @param theta A single positive real number saved as a \code{numeric} object.
#'              By default, \code{theta = 1}.
#'
#' @return 
#' This function returns the calculated sample Fréchet variance of the given
#' \eqn{n} interval, which is defined as a non-negative real number. Therefore,
#' the output of this function is a single \code{numeric} object.
#'
#' @details
#' Let \eqn{\mathcal{X}} be an interval-valued random set
#' and let \eqn{\left(x_{1},x_{2},\ldots,x_{n}\right)} be a sample of \eqn{n}
#' independent observations drawn from \eqn{\mathcal{X}}. Then, the sample
#' Fréchet variance (see Fréchet, 1948) is defined as the following
#' non-negative real number given by
#' \deqn{s_{\mathcal{X}}^{2} =
#' \frac{1}{n}\sum_{i=1}^{n}d_{\theta}^{2}\left(x_{i}, \overline{x}\right),}
#' where \eqn{\theta>0} and \eqn{\overline{x}} denotes the sample Aumann mean
#' of \eqn{\left(x_{1},x_{2},\ldots,x_{n}\right)}. Due to \eqn{\theta}-distance
#' definition, this deviation measure can also be computed as follows,
#' \deqn{s_{\mathcal{X}}^{2} =
#' s_{\mathrm{mid}~\mathcal{X}}^{2}+\theta\cdot
#' s_{\mathrm{spr}~\mathcal{X}}^{2},}
#' where 
#' \deqn{s_{\mathrm{mid}~\mathcal{X}} = \frac{1}{n}\sum_{i=1}^{n}
#' (\mathrm{mid}~x_{i} - \mathrm{mid}~\overline{x})^{2},}
#' \deqn{s_{\mathrm{spr}~\mathcal{X}} = \frac{1}{n}\sum_{i=1}^{n}
#' (\mathrm{spr}~x_{i} - \mathrm{spr}~\overline{x})^{2}.}
#'
#' @exportMethod var
#' @docType methods
#' @name var
#' @rdname var-methods
#' @aliases var,IntervalList-method
#'
#' @usage
#' \S4method{var}{IntervalList}(x, theta = 1)
#'
#' @references
#' Fréchet, M. (1948). Les éléments aléatoires de nature quelconque dans un
#' espace distancié. \emph{Annales de l'institut Henri Poincaré}, 10(4):215-310.
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @seealso
#' Other sample central tendency and covariance measures such as sample
#' Aumann mean and sample covariance can be calculated through
#' \code{\link{mean}()} and \code{\link{cov}()} functions, respectively. 
#'
#' @examples
#' ## Some var() examples
#' list <- IntervalList(c(1, 3), c(2, 5))
#' var(list)
#' var(list, theta = 1/3)

setGeneric("var",
           function(x, theta = 1, y = NULL, na.rm = FALSE, use, ...)
             stats::var(x, y, na.rm, use))

setMethod("var",
          signature(x = "IntervalList"),
          function(x, theta = 1)
          {
            if (! is_single_positive_real_number(theta))
              stop("'theta' argument should be a single positive real number")

            n <- length(x)
            return((n - 1) * var(x@mid) / n +
                     theta * (n - 1) * var(x@spr) / n)
          }
)
