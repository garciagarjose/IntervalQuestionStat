
## This file is part of the IntervalQuestionStat package

#' @title
#' Estimate Cronbach's \eqn{\alpha} coefficient
#'
#' @description
#' Calculates Cronbach's \eqn{\alpha} coefficient as an estimate of reliability
#' for Likert-type, visual analogue and interval-valued data scales responses.
#'
# #' @usage
# #' \S4method{cronbach}{data.frame,logical,numeric,numeric}(data, ivd = TRUE, type = 1, theta = 1)
#'
#' @param data a matrix or dataframe.
#' @param ivd a logical value indicating if an interval-valued scale is used (default) or not.
#' @param type a number specifying the order and the characterisation that is being used.
#' Only four options are allowed:
#' \itemize{
#'    \item \code{1}: inf/sup-characterisation is used variable by variable (default).
#'    \item \code{2}: mid/spr-characterisation is used variable by variable.
#'    \item \code{3}: all infimums are followed by all supremums in the same variable order.
#'    \item \code{4}: all mid-points are followed by all spreads in the same variable order.
#' }
#' @param theta a single positive numeric value. By default, \code{theta}=1.
#'
#' @references
#' Cronbach L.J. (1951). Coefficient alpha and the internal structure of tests. Psychometrika, 16, 297-334.
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es}
#' 
#' @return Returns the calculated Cronbach's \eqn{\alpha} coefficient for the given dataset.
#' 
#' @details
#' For Likert-type and visual analogue scale responses the Cronbach's \eqn{\alpha} coefficient computed by \code{cronbach()} is defined as follows,
#' \deqn{\alpha = \frac{k}{k - 1}\left(1 - \frac{\sum_{j=1}^k s_{X_{j}}^2}{s_{X_{total}}^2}\right),}
#' where \eqn{k>1} is the number of items, \eqn{s_{X_{j}}^2} is the sample variance of the \eqn{j}-th
#' item and \eqn{s_{X_{total}}^2} is the sample variance of the sum of all the involved items, that is,
#' \deqn{X_{total}=X_{1}+X_{2}+\ldots +X_{k}.}
#' 
#' Analogously, for interval-valued scale responses the Cronbach's \eqn{\alpha} coefficient computed by this function is defined as follows,
#' \deqn{\alpha = \frac{k}{k - 1}\left(1 - \frac{\sum_{j=1}^k s_{\mathcal{X}_{j}}^2}{s_{\mathcal{X}_{total}}^2}\right),}
#' where \eqn{k>1} is the number of items, \eqn{s_{\mathcal{X}_{j}}^2} is the sample Frechet variance of the \eqn{j}-th
#' item and \eqn{s_{\mathcal{X}_{total}}^2} is the sample Frechet variance of the sum of all the involved items, that is,
#' \deqn{\mathcal{X}_{total}=\mathcal{X}_{1}+\mathcal{X}_{2}+\ldots +\mathcal{X}_{k}.}
#'
#' @examples
#' ## Calculate Cronbach's alpha coefficient for interval-valued responses
#' data1 <- matrix(c(1, 1.5, 3.8, 4, 2.6, 3, 6, 7), 2, 4, byrow = TRUE)
#' cronbach(data1)
#'
#' ## Calculate Cronbach's alpha coefficient for Likert-type responses
#' data2 <- ivd2likert(IntervalMatrix(data1))
#' cronbach(data2, ivd = FALSE)
#'
#' ## Calculate Cronbach's alpha coefficient for visual analogue responses
#' data3 <- ivd2vas(IntervalMatrix(data1))
#' cronbach(data3, ivd = FALSE)
#' 
#' 
#' ## Real-life example
#' ## Load the data
#' data(lackinfo, package = "IntervalQuestionStat")
#' 
#' ## Calculate Cronbach's alpha coefficient for interval-valued responses
#' cronbach(lackinfo[, 4:13])
#' 
#' ## Convert to Likert-type responses and calculate Cronbach's alpha coefficient
#' cronbach(ivd2likert(IntervalMatrix(lackinfo[, 4:13])), ivd = FALSE)
#' 
#' ## Convert to visual analogue responses and calculate Cronbach's alpha coefficient
#' cronbach(ivd2vas(IntervalMatrix(lackinfo[, 4:13])), ivd = FALSE)
#'
#' @export

cronbach <- function(data,
                     ivd = TRUE,
                     type = 1,
                     theta = 1)
{
  if (ivd == FALSE)
  {
    k <- ncol(data)
    vartotal <- var(apply(data, 1, sum))
    sumvar <- sum(apply(data, 2, var))
  } else {
    m <- IntervalMatrix(data, type = type)
    k <- ncol(m)

    if (k == 1) stop("number of variables must be greater than 1")

    vartotal <- var(apply(m, 1, sum),
                    theta)

    sumvar <- sum(apply(m, 2, function(x) var(x, theta)))
  }

  alpha  <- k*(1-sumvar/vartotal)/(k-1)
  return(alpha)
}
