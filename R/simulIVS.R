
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Simulation of interval-valued responses to a questionnaire
#'
#' @description
#' This function allows to generate \eqn{n} interval-valued responses to each
#' of \eqn{k} items of a questionnaire. These interval-valued responses are
#' simulated mimicking the human behavior, considering three different
#' procedures as it is suggested by De la Rosa de Sáa \emph{et al}. (2015)
#' and Lubiano \emph{et al}. (2021).
#'
#' @param n A single positive integer indicating the number of different
#'          respondents that have answered to the questionnaire stored as
#'          a unique \code{numeric} object.
#' @param k A single positive integer indicating the number of different items
#'          included in the questionnaire stored as a unique \code{numeric}
#'          object.
#' @param w1 A single real number in \eqn{[0,1]} indicating the proportion
#'           of observations that are generated in the first procedure
#'           stored as a unique \code{numeric} object. By default,
#'           \code{w1 = 0.05}.
#' @param w2 A single real number in \eqn{[0,1]} indicating the proportion
#'           of observations that are generated in the second procedure
#'           saved as a unique \code{numeric} object. By default,
#'           \code{w2 = 0.35}.
#' @param w3 A single real number in \eqn{[0,1]} indicating the proportion
#'           of observations that are generated in the third procedure
#'           stored as a unique \code{numeric} object. By default,
#'           \code{w3 = 0.60}.
#' @param p A single non-negative real number which indicates the first
#'          parameter of a beta distribution. By default, \code{p = 2}.
#' @param q A single non-negative real number which indicates the second
#'          parameter of a beta distribution. By default, \code{q = 2}.
#' @param minimum A single real number indicating the lower bound of the
#'                interval-valued scale used saved as a unique \code{numeric}
#'                value. By default, \code{minimum = 1}.
#' @param maximum A single real number indicating the upper bound of the
#'                interval-valued scale used saved as a unique \code{numeric}
#'                value. By default, \code{maximum = 7}.
#'
#' @return 
#' This function returns \eqn{n} interval-valued responses to each of \eqn{k}
#' items in a questionnaire contained in a \code{data.frame} with \eqn{n} rows
#' and \eqn{2k} columns with values in the reference interval
#' \eqn{[\code{minimum}, \code{maximum}]}. All interval-valued data's lower
#' bounds appear in the first \eqn{k} columns of the data.frame and then all
#' the corresponding upper bounds appear too.
#'
#' @references
#' \itemize{
#'    \item De la Rosa de Sáa, S.; Gil, M.Á.; González-Rodríguez, G.;
#'          López, M.T.; Lubiano M.A. (2015). Fuzzy rating scale-based
#'          questionnaires and their statistical analysis, \emph{IEEE
#'          Transactions on Fuzzy Systems}, 23(1):111-126.
#'          \doi{10.1109/TFUZZ.2014.2307895}.
#'    \item Lubiano, M.A.; García-Izquierdo, A.L.; Gil, M.Á. (2021). Fuzzy
#'          rating scales: Does internal consistency of a measurement scale
#'          benefit from coping with imprecision and individual differences in
#'          psychological rating? \emph{Information Sciences}, 550:91-108.
#'          \doi{10.1016/j.ins.2020.10.042}.
#' }
#'
#' @author José García-García \email{garciagarjose@@uniovi.es},\cr
#' with contributions from María Asunción Lubiano \email{lubiano@@uniovi.es}
#' 
#' @export
#'
#' @examples
#' ## Simulation some interval-valued responses to a questionnaire
#' ## carried out under the following particular conditions
#' ## Number of respondents: n = 100
#' ## Number of items: k = 5
#' ## Procedures proportions: (w1, w2, w3) = (0.10, 0.25, 0.65)
#' ## Beta distribution parameters: (p, q) = (1, 7)
#' ## Reference interval of interval-valued scales: [0, 10]
#' data <- simulIVS(100, 5, 0.10, 0.25, 0.65, 1, 7, 0, 10)
#' head(data)


simulIVS <- function(n, k, w1 = 0.05, w2 = 0.35, w3 = 0.6,
                     p = 2, q = 2, minimum = 1, maximum = 7)
{
  if (! is_single_positive_integer(n))
    stop("'n' argument should be a single positive integer")
  
  if (! is_single_positive_integer(k))
    stop("'k' argument should be a single positive integer")
  
  if (! is_single_proportion(w1))
    stop("'w1' argument should be a single real number in [0, 1]")
  
  if (! is_single_proportion(w2))
    stop("'w2' argument should be a single real number in [0, 1]")
  
  if (! is_single_proportion(w3))
    stop("'w3' argument should be a single real number in [0, 1]")
  
  if (sum(c(w1, w2, w3)) != 1)
    stop("It should be fulfilled that w1 + w2 + w3 = 1")
  
  if (length(p) != 1 || p < 0)
    stop("'p' argument should be a single non-negative real number")
  
  if (length(q) != 1 || q < 0)
    stop("'q' argument should be a single non-negative real number")
  
  if (! is_single_real_number(minimum))
    stop("'minimum' argument should be a single real number")
  
  if (! is_single_real_number(maximum))
    stop("'maximum' argument should be a single real number")
  
  if (minimum > maximum)
    stop("'minimum' argument should be less or equal than 'maximum' argument")
  
  nh <- n * c(w1, w2, w3)
  integers <- floor(nh)
  decimals <- nh - integers
  while (sum(integers) < n)
  {
    position <- which.max(decimals)
    integers[position] <- integers[position] + 1
    decimals[position] <- 0
  }
  
  M1 <- matrix(rbeta(integers[1] * 2, p, q), nrow = integers[1], ncol = 2)
  M1 <- t(apply(M1, 1, sort))
  
  mid2 <- rbeta(integers[2], p, q)
  spr2 <- vapply(mid2,
                 function(x) runif(1, 0, min(1/3, x, 1 - x)),
                 numeric(1))
  
  mid3 <- rbeta(integers[3], p, q)
  spr3 <- rgamma(integers[3], 2, 20)
  
  auxinfs <- c(M1[, 1], mid2 - spr2, mid3 - spr3)
  auxsups <- c(M1[, 2], mid2 + spr2, mid3 + spr3)
  
  auxinfs[which(auxinfs < 0)] <- 0
  auxinfs[which(auxinfs > 1)] <- 1
  auxsups[which(auxsups < 0)] <- 0
  auxsups[which(auxsups > 1)] <- 1
  
  a <- runif(k)
  b <- matrix(rep(rnorm(k, 0.1, 0.05), each = n), n, k)
  e <- matrix(rnorm(n * k, 0.1, 0.05), n, k)
  
  infs <- outer(auxinfs, a) + b + e
  sups <- outer(auxsups, a) + b + e
  
  infs[which(infs < 0)] <- 0
  infs[which(infs > 1)] <- 1
  sups[which(sups < 0)] <- 0
  sups[which(sups > 1)] <- 1
  
  infs <- (maximum - minimum) * infs + minimum
  sups <- (maximum - minimum) * sups + minimum
  
  output <- as.data.frame(cbind(infs, sups))
  names(output) <- c(paste0("inf", 1:k),
                     paste0("sup", 1:k))
  
  return(output)
}
