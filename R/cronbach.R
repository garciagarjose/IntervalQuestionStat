
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Estimate sample Cronbach's \eqn{\alpha} coefficient
#'
#' @description
#' This function allows to calculate the sample Cronbach's \eqn{\alpha}
#' coefficient as an estimate of the reliability (understood in this case under
#' the internal consistency point of view) of the responses collected through
#' Likert-type, visual analogue, and interval-valued rating scales in
#' questionnaires.
#'
#' @param data A \code{matrix} or \code{data.frame} with the
#'             questionnaire's responses.
#' @param ivs A \code{logical} value indicating if an
#'            interval-valued scale is used (default) or not.
#' @param type A single \code{numeric} value specifying both the order
#'             and the characterization that is being used for storing
#'             interval-valued information on \code{data} argument.
#'             Only the following four options are allowed:
#'             \itemize{
#'                 \item \code{1}: The \emph{inf/sup}-characterization is
#'                       used variable by variable (default).
#'                 \item \code{2}: The \emph{mid/spr}-characterization is
#'                       used variable by variable.
#'                 \item \code{3}: All the supremums follow all the
#'                       infimums in the same variable order.
#'                 \item \code{4}: All the spreads follow all the
#'                       mid-points in the same variable order.
#'             }
#' @param theta A single positive real number stored as a unique \code{numeric}
#'              value which is used for distance computations. By default,
#'              \code{theta = 1}.
#'
#' @references
#' Cronbach L.J. (1951). Coefficient alpha and the internal structure of tests.
#' \emph{Psychometrika}, 16, 297-334. \doi{1001007/BF02310555}.
#'
#' @author José García-García \email{garciagarjose@@uniovi.es}
#' 
#' @return 
#' This function returns the calculated sample Cronbach's
#' \eqn{\alpha} coefficient stored as a single \code{numeric}
#' value for measuring the reliability or internal consistency
#' of the given questionnaire's responses.
#' 
#' @details
#' For both traditional Likert-type and visual analogue rating scales responses,
#' the sample Cronbach's \eqn{\alpha} coefficient (Cronbach, 1951) computed by
#' \code{cronbach()} function is defined as follows,
#' \deqn{\widehat{\alpha} = \frac{k}{k - 1}\left(1 -
#' \frac{\sum_{j=1}^{k} s_{X_{j}}^2}{s_{X_{total}}^2}\right),}
#' where \eqn{k>1} is the number of items; \eqn{s_{X_{j}}^2} is the sample
#' variance of \eqn{X_{j}}, which is the real-valued random variable
#' modeling the responses to the \eqn{j}-th item; and \eqn{s_{X_{total}}^2}
#' is the sample variance of the sum of all the involved items, that is,
#' \deqn{X_{total}=X_{1}+X_{2}+\ldots +X_{k}.}
#' 
#' Analogously, for interval-valued scale responses the sample Cronbach's
#' \eqn{\alpha} coefficient computed by this function is defined as follows,
#' \deqn{\widehat{\alpha} = \frac{k}{k - 1}\left(1 - \frac{\sum_{j=1}^{k}
#' s_{\mathcal{X}_{j}}^2}{s_{\mathcal{X}_{total}}^2}\right),}
#' where \eqn{k>1} is the number of items; \eqn{s_{\mathcal{X}_{j}}^2} is the
#' sample Fréchet variance of \eqn{\mathcal{X}_{j}}, which is the
#' interval-valued random set modeling the responses to the \eqn{j}-th item; and
#' \eqn{s_{\mathcal{X}_{total}}^2} is the sample Fréchet variance of the sum of
#' all the involved items, that is, \deqn{\mathcal{X}_{total} =
#' \mathcal{X}_{1}+\mathcal{X}_{2}+\ldots +\mathcal{X}_{k}.}
#'
#' @examples
#' ## These code lines illustrate Cronbach's alpha coefficient calculation
#' ## for interval-valued, Likert-type, and visual analogue scales responses
#' 
#' ## Some trivial cronbach() examples
#' ## Cronbach's alpha index for interval-valued scale responses stored
#' ## in a matrix with the inf/sup-characterization variable by variable
#' ## using Bertoluzza's distance with Lebesgue measure (theta = 1/3)
#' data1 <- matrix(c(1, 2.6, 1.5, 3, 3.8, 6, 4, 7), 2, 4)
#' cronbach(data1, theta = 1/3)
#' 
#' ## Cronbach's alpha index for interval-valued scale responses stored
#' ## in a data.frame with the mid/spr-characterization saving all the
#' ## mid-points and then all the spreads using rho2 distance (theta = 1)
#' data2 <- data.frame(mids1 = c(2, 3),
#'                     mids2 = c(4, 5),
#'                     sprs1 = c(1, 2),
#'                     sprs2 = c(2, 4))
#' cronbach(data2, type = 4)
#' 
#' ## Cronbach's alpha coefficient for Likert-type
#' ## scale responses stored in a matrix
#' data3 <- matrix(c(1, 3, 4, 7), 2, 2)
#' cronbach(data3, ivs = FALSE)
#'
#' ## Cronbach's alpha coefficient for visual analogue
#' ## scale responses stored in a data.frame
#' data4 <- data.frame(item1 = c(1.5, 2.8),
#'                     item2 = c(3.9, 6.2))
#' cronbach(data4, ivs = FALSE)
#' 
#' ## Real-life data example
#' ## Load the interval-valued data
#' data(lackinfo, package = "IntervalQuestionStat")
#' 
#' ## Calculate Cronbach's alpha coefficient for interval-valued responses
#' cronbach(lackinfo[, 3:12])
#' 
#' ## Convert interval-valued responses into their corresponding equivalent
#' ## Likert-type answers and then calculate Cronbach's alpha coefficient
#' likert <- ivs2likert(IntervalMatrix(lackinfo[, 3:12]))
#' cronbach(likert, ivs = FALSE)
#' 
#' ## Analogously, interval-valued responses are transformed into their
#' ## corresponding equivalent visual analogue scale answers and
#' ## Cronbach's alpha coefficient is then computed
#' vas <- ivs2vas(IntervalMatrix(lackinfo[, 3:12]))
#' cronbach(vas, ivs = FALSE)
#'
#' @export

cronbach <- function(data,
                     ivs = TRUE,
                     type = 1,
                     theta = 1)
{
  if (length(ivs) != 1 || ! is.logical(ivs))
    stop("'ivs' argument should be a single logical object")
  
  if (! type %in% 1:4)
    stop("'type' argument should be one of 1, 2, 3 or 4")
  
  if ( !is.data.frame(data) && !is.matrix(data) )
    stop("'x' must be a data.frame or a matrix")
  
  if (! is_single_positive_real_number(theta))
    stop("'theta' argument should be a single positive real number")
  
  if (ivs == FALSE)
  {
    k <- ncol(data)
    vartotal <- var(apply(data, 1, sum))
    sumvar <- sum(apply(data, 2, var))
  } else {
    m <- IntervalMatrix(data, type = type)
    k <- ncol(m)

    if (k == 1)
      stop("Number of variables should be greater than 1")

    vartotal <- var(apply(m, 1, sum),
                    theta)

    sumvar <- sum(apply(m, 2, function(x) var(x, theta)))
  }

  alpha  <- k * (1 - sumvar / vartotal) / (k - 1)
  return(alpha)
}
