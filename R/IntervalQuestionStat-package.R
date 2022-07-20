
## This file is part of the IntervalQuestionStat package

#' @title Tools to Deal with Interval-Valued Responses in Questionnaires
#'
#' @description
#' \pkg{IntervalQuestonStat} is an open source package for R.
#' It provides S4 classes and methods to deal with interval-valued responses in questionnaires.
#' It also includes some basic functions for doing the statistical analysis of this type of data.
#'
#' @details
#' In Social and Educational Sciences and many other disciplines, interval-valued
#' scales arise as an alternative to traditional Likert-type or visual analogue scales
#' in some questionnaires measuring people's behaviour (attitudes, opinions, perceptions, feelings, etc.).
#' This type of data cannot be numerically measured because they concern intrinsically imprecise valued
#' attributes. Likert-type and visual analogue scales force to choose a single point response linked
#' to a statement or question, so individual differences are almost systematically overlooked.
#' To overcome the limitations of these traditional scales in capturing uncertainty of respondent answers,
#' interval-valued scales allow respondents to select a range or interval of real data and not being constrained to a single point.
#'
#' The package aims to provide the following functionality:
#' \enumerate{
#'    \item Calculation of basic operations with interval-valued data
#'          (see \link{arithmetic} and \code{\link{distance}}).
#'    \item Calculation of some central tendency and variation measures
#'          (see \code{\link{mean}}, \code{\link{var}} and \code{\link{cov}}).
#'    \item Visualization of interval-valued data (see \code{\link{plot}}).
#'    \item Transformation of interval-valued responses into Likert-type or
#'          visual analogue responses (see \code{\link{ivd2likert}} and \code{\link{ivd2vas}}).
#'    \item Statistical analysis of reliability of questionnaire's responses
#'          (see \code{\link{cronbach}}).
#'    \item Simulation of interval-valued responses in questionnaires
#'          (see \code{\link{simulIVS}}).
#' }
#'
#' For a complete list of classes and methods
#' call \code{help(package="IntervalQuestionStat")}.
#' \cr\cr
#'
#' \bold{Acknowledgments}: The development of this package in May-June 2022 was partially supported
#'  by Principality of Asturias Grant AYUD/2021/50897 and the Spanish Ministry of Economy and
#'  Business Grant PID2019-104486GB-I00.
#'
#' @name IntervalQuestionStat-package
#'
#' @docType package
#'
#' @author Jose Garcia Garcia \email{garciagarjose@@uniovi.es},\cr
#' with contributions from Asun Lubiano \email{lubiano@@uniovi.es}.
#'
#' @references
#' \itemize{
#'    \item Aumann, R.J. (1965). Integrals of set-valued functions. Journal of Mathematical Analysis
#'    and Applications, 12(1):1-12.
#'    \item Cronbach L.J. (1951). Coefficient alpha and the internal structure of tests. Psychometrika, 16, 297-334.
#'    \item De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A. (2015). Fuzzy rating scale-based questionnaires and their
#'    statistical analysis, IEEE Transactions on Fuzzy Systems, 23(1):111-126.
#'    \item Frechet, M. (1948). Les elements aleatoires de nature quelconque dans un espace distancie.
#'    Annales de l'institut Henri Poincare, 10(4):215-310.
#'    \item Gil, M.A.; Lubiano, M.A.; Montenegro, M.; Lopez, M.T. (2002).
#'    Least squares fitting of an affine function and strength of association for interval-valued data.
#'    Metrika 56:97-111.
#'    \item Hankin, R.K.S. (2010). A step-by-step guide to writing a simple package that uses S4 methods:
#'    a "hello world" example. Technical Report. Auckland University of Technology.
#'    \item Lubiano, M.A.; Garcia-Izquierdo, A.L.; Gil, M.A. (2021). Fuzzy rating scales: Does internal consistency of a measurement scale benefit
#'    from coping with imprecision and individual differences in psychological rating? Information Sciences, 550:91-108.
#'    \item Minkowski, H. (1903). Volumen und oberflache. Mathematische Annalen, 57:447-495.
#'    \item Moore, R.E.; Kearfott, R.B.; Cloud, M.J. (2009). Introduction to Interval Analysis. Society
#'    for Industrial and Applied Mathematics, USA.
#' }
#'
#' @import methods
#' @import grDevices
#' @import graphics
#' @import stats

invisible(NULL)
