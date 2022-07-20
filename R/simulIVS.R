
## This file is part of the IntervalQuestionStat package

#' @title
#' Simulation of interval-valued responses to a questionnaire
#'
#' @description
#' This function generates \eqn{n} interval-valued responses to each of \eqn{k} items of a questionnaire.
#' These interval-valued responses are simulated mimicking the human behavior, considering for it
#' a finite mixture of three different procedures adapting the ideas of De la Rosa de Saa et al. (2015) and Lubiano et al. (2021).
#'
#' @param n a positive integer indicating the number of observations in the questionnaire.
#' @param k a single positive integer indicating the number of items in the questionnaire.
#' @param w1 a single numeric value in \eqn{[0,1]} indicating the proportion of observations that are generated in the first procedure. By default, \code{w1}=0.05.
#' @param w2 a single numeric value in \eqn{[0,1]} indicating the proportion of observations that are generated in the second procedure. By default, \code{w2}=0.35.
#' @param w3 a single numeric value in \eqn{[0,1]} indicating the proportion of observations that are generated in the third procedure. By default, \code{w3}=0.60.
#' @param p a single positive numeric value indicating the first parameter of a beta distribution. By default, \code{p}=2.
#' @param q a positive numeric value indicating the second parameter of the beta distribution. By default, \code{q}=2.
#' @param minimum a single numeric value indicating the lower bound of the interval-valued scale used in the questionnaire. By default, \code{minimum}=1.
#' @param maximum a single numeric value indicating the upper bound of the interval-valued scale used in the questionnaire. By default, \code{maximum}=7.
#'
#' @return This function returns \eqn{n} interval-valued responses to each of \eqn{k} items in a questionnaire contained in a dataframe with \eqn{n} rows and \eqn{2k} columns
#' with values in the interval \eqn{[\code{minimum}, \code{maximum}]}. All interval-valued data's infimums appear in the first \eqn{k} columns of the dataframe and then
#' appear its corresponding supremums.
#'
#' @references
#' \itemize{
#'    \item De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A. (2015). Fuzzy rating scale-based questionnaires and their
#'    statistical analysis, IEEE Transactions on Fuzzy Systems, 23(1):111-126.
#'    \item Lubiano, M.A.; Garcia-Izquierdo, A.L.; Gil, M.A. (2021). Fuzzy rating scales: Does internal consistency of a measurement scale benefit
#'    from coping with imprecision and individual differences in psychological rating? Information Sciences, 550:91-108.
#' }
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es},\cr
#' with contributions from Asun Lubiano \email{lubiano@@uniovi.es}
#'
#' @examples
#' data <- simulIVS(100, 5)
#' head(data)
#'
#' @export

simulIVS <- function(n, k, w1 = 0.05, w2 = 0.35, w3 = 0.6, p = 2, q = 2, minimum = 1, maximum = 7)
{
  if (sum(c(w1, w2, w3)) != 1) stop("it should be fulfilled that w1+w2+w3=1")

  nh <- n*c(w1, w2, w3)
  integers <- floor(nh)
  decimals <- nh - integers
  while (sum(integers) < n)
  {
    position <- which.max(decimals)
    integers[position] <- integers[position] + 1
    decimals[position] <- 0
  }

  M1 <- matrix(rbeta(integers[1]*2, p, q), nrow = integers[1], ncol = 2)
  M1 <- t(apply(M1, 1, sort))

  mid <- rbeta(integers[2], p, q)
  spr <- sapply(mid,
                function(x) runif(1, 0, min(1/3, x, 1-x)))
  M2 <- cbind(mid-spr, mid+spr)

  mid <- rbeta(integers[3], p, q)
  spr <- rgamma(integers[3], 2, 20)
  M3 <- cbind(mid-spr, mid+spr)

  M <- rbind(M1, M2, M3)
  auxintervalList <- IntervalList(M, type = 1)

  infs <- matrix(nrow = n, ncol = k)
  sups <- matrix(nrow = n, ncol = k)

  a <- runif(k)
  b <- rnorm(k, 0.1, 0.05)

  for (i in 1:n)
  {
    for (j in 1:k)
    {
      interval <- a[j]*auxintervalList[[i]] + b[j] + rnorm(1, 0.1, 0.05)
      inf <- interval@mid - interval@spr
      sup <- interval@mid + interval@spr

      if (inf < 0) inf <- 0
      if (inf > 1) inf <- 1
      if (sup < 0) sup <- 0
      if (sup > 1) sup <- 1

      myinterval <- (maximum-minimum)*IntervalData(inf, sup)+minimum
      infs[i,j] <- myinterval@mid - myinterval@spr
      sups[i,j] <- myinterval@mid + myinterval@spr
    }
  }

  output <- as.data.frame(cbind(infs, sups))
  names(output) <- c(paste0("inf", 1:k),
                     paste0("sup", 1:k))

  return(output)

}
