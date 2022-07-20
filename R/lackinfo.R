
## This file is part of the IntervalQuestionStat package

#' @title
#' Lack of information in expository face-to-face lessons questionnaire dataset
#'
#' @description
#' A dataset containing some demographic data and the responses
#' to 5 items measuring the perception of lack of information in
#' expository face-to-face lessons obtained with a questionnaire.
#' 
#' @details
#' An educational innovation project was carried out for improving
#' teaching-learning processes at the University of Oviedo (Spain)
#' for the 2020/2021 academic year. A total of 50 students have been
#' requested to answer an online questionnaire about some demographic
#' data (sex and age) and their perception of lack of information in
#' expository face-to-face lessons by selecting the
#' interval that best represents their level of agreement to the
#' statements proposed in a interval-valued scale bounded between 1 and 7,
#' where 1 represents the option 'strongly disagree' and 7 represents the option
#' 'strongly agree'.
#' 
#' These are the 5 items used to measure the perception of lack of information in expository face-to-face lessons:
#' \itemize{
#'     \item I1: I receive too little information from my classmates.
#'     \item I2: It is difficult to receive relevant information from my classmates.
#'     \item I3: It is difficult to receive relevant information from the teacher.
#'     \item I4: The amount of information I receive from my classmates is very low.
#'     \item I5: The amount of information I receive from the teacher is very low.
#' }
#' 
#' @format
#' A data frame with 50 observations of the following 13 variables:
#' \itemize{
#'     \item \code{id}: identification number.
#'     \item \code{sex}: sex of the respondent (\code{male} or \code{female}).
#'     \item \code{age}: respondent's age (in years).
#'     \item \code{inf1}: infimum of respondent's interval-valued answer to item 1.
#'     \item \code{sup1}: supremum of respondent's interval-valued answer to item 1.
#'     \item \code{inf2}: infimum of respondent's interval-valued answer to item 2.
#'     \item \code{sup2}: supremum of respondent's interval-valued answer to item 2.
#'     \item \code{inf3}: infimum of respondent's interval-valued answer to item 3.
#'     \item \code{sup3}: supremum of respondent's interval-valued answer to item 3.
#'     \item \code{inf4}: infimum of respondent's interval-valued answer to item 4.
#'     \item \code{sup4}: supremum of respondent's interval-valued answer to item 4.
#'     \item \code{inf5}: infimum of respondent's interval-valued answer to item 5.
#'     \item \code{sup5}: supremum of respondent's interval-valued answer to item 5.
#' }
#' 
#' @examples 
#' data(lackinfo, package = "IntervalQuestionStat")
#' head(lackinfo)
#' summary(lackinfo)

"lackinfo"