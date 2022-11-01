
## This file is part of the IntervalQuestionStat package for R

#' @title
#' Tools to Deal with Interval-Valued Responses in Questionnaires
#'
#' @description
#' \pkg{IntervalQuestonStat} is an open source package for doing the statistical
#' analysis of interval-valued responses collected through interval-valued
#' scales in lots of different widely used questionnaires measuring many
#' intrinsically imprecise human attributes in the \proglang{R} environment.
#' In particular, this package implements the theoretical concepts, results,
#' and ideas suggested by the SMIRE+CoDiRE (\emph{Statistical Methods with
#' Imprecise Random Elements and Comparison of Distributions of Random
#' Elements}) Research Group
#' (\url{https://bellman.ciencias.uniovi.es/smire+codire/}) from the University
#' of Oviedo (Spain) taking into account some applied investigations and
#' real-life studies.
#'
#' @details
#' In Social and Educational Sciences and many other disciplines,
#' interval-valued scales arise as a strong alternative to both traditional
#' Likert-type or visual analogue scales in some questionnaires measuring
#' people's behavior (attitudes, opinions, perceptions, feelings, etc.).
#' This type of data can not be directly measured because it concerns inherently
#' imprecise features. Likert-type and visual analogue scales force respondents
#' to choose single-point answers linked to some items (statements or
#' questions), so individual differences are almost systematically overlooked.
#' In order to overcome the limitations of these scales in capturing uncertainty
#' over respondents' answers, interval-valued scales allow them to select a
#' real-valued interval and not being constrained to a single point.
#' 
#' This package  provides S4 classes, methods, and functions for dealing
#' with this type of data and it also includes some real-life data sets.
#' In particular, it aims to provide the following functionality:
#' \enumerate{
#'    \item Definition of interval-valued objects and instances (see
#'          \link{IntervalData-class}, \code{\link{IntervalData}},
#'          \link{IntervalList-class}, \code{\link{IntervalList}},
#'          \link{IntervalMatrix-class}, and \code{\link{IntervalMatrix}}).
#'    \item Calculation of basic operations with interval-valued data
#'          (see \link{arithmetic} and \code{\link{distance}}).
#'    \item Calculation of some central tendency and variation measures
#'          (see \code{\link{mean}}, \code{\link{var}} and \code{\link{cov}}).
#'    \item Visualization of interval-valued data (see \code{\link{plot}}).
#'    \item Association of interval-valued responses and their corresponding
#'          equivalent Likert-type and visual analogue scales responses (see
#'          \code{\link{ivs2likert}} and \code{\link{ivs2vas}}).
#'    \item Statistical analysis of reliability of questionnaire's responses
#'          (see \code{\link{cronbach}}).
#'    \item Simulation of interval-valued responses in questionnaires
#'          (see \code{\link{simulIVS}}).
#' }
#'
#' For a complete list of classes, methods and functions included in the
#' \pkg{IntervalQuestionStat} package call
#' \code{help(package="IntervalQuestionStat")} on the \proglang{R} console.
#' \cr
#'
#' \bold{Acknowledgments}: The initial development of this \proglang{R} package
#' has been partially supported by the Principality of Asturias Grant
#' AYUD/2021/50897 and also by the Spanish Ministry of Economy and
#' Business Grant PID2019-104486GB-I00.
#'
#' @name IntervalQuestionStat-package
#'
#' @docType package
#'
#' @author José García-García \email{garciagarjose@@uniovi.es},\cr
#' with contributions from María Asunción Lubiano \email{lubiano@@uniovi.es}.
#'
#' @references
#' \itemize{
#'    \item Aumann, R.J. (1965). Integrals of set-valued functions.
#'          \emph{Journal of Mathematical Analysis and Applications},
#'          12(1):1-12. \doi{10.1016/0022-247X(65)90049-1}.
#'    \item Cronbach L.J. (1951). Coefficient alpha and the internal structure
#'          of tests. \emph{Psychometrika}, 16, 297-334.
#'          \doi{1001007/BF02310555}.
#'    \item De la Rosa de Sáa, S.; Gil, M.Á.; González-Rodríguez, G.;
#'          López, M.T.; Lubiano M.A. (2015). Fuzzy rating scale-based
#'          questionnaires and their statistical analysis, \emph{IEEE
#'          Transactions on Fuzzy Systems}, 23(1):111-126.
#'          \doi{10.1109/TFUZZ.2014.2307895}.
#'    \item Fréchet, M. (1948). Lés éléments aléatoires de nature quelconque
#'          dans un espace distancié. \emph{Annales de l'institut Henri
#'          Poincaré}, 10(4):215-310.
#'    \item Gil, M.Á.; Lubiano, M.A.; Montenegro, M.; López, M.T. (2002). Least
#'          squares fitting of an affine function and strength of association
#'          for interval-valued data. \emph{Metrika}, 56:97-111.
#'          \doi{10.1007/s001840100160}.
#'    \item Hankin, R.K.S. (2010).. A step-by-step guide to writing a simple
#'          package that uses S4 methods: a "hello world" example. Technical
#'          Report. Auckland University of Technology.
#'    \item Lubiano, M.A.; García-Izquierdo, A.L.; Gil, M.Á. (2021). Fuzzy
#'          rating scales: Does internal consistency of a measurement scale
#'          benefit from coping with imprecision and individual differences in
#'          psychological rating?. \emph{Information Sciences}, 550:91-108.
#'          \doi{10.1016/j.ins.2020.10.042}.
#'    \item Minkowski, H. (1903). Volumen und oberfläche. \emph{Mathematische
#'          Annalen}, 57:447-495.
#'    \item Moore, R.E.; Kearfott, R.B.; Cloud, M.J. (2009). Introduction to
#'          Interval Analysis. \emph{Society for Industrial and Applied
#'          Mathematics}, USA. \doi{10.1137/1.9780898717716}.
#' }
#'
#' @import methods
#' @import grDevices
#' @import graphics
#' @import stats

invisible(NULL)
