#' Download ACS Summary File Data and Build Database.
#'
#' Download ACS Summary File data from FTP site, clean data and create tables in
#' database.
#'
#' @docType package
#' @name acssf
#' @importFrom dplyr %>%
#' @importFrom glue glue
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
