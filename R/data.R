
#' Summary levels and ACS surveys
#'
#' A dataset containing the summary levels supported by [acs_extract_raw()] and the ACS surveys for which they are available.
#'
#' @format A data frame with 8 rows and 4 variables:
#' * geo_type: Geography type
#' * sum_level: Summary level code
#' * spans: ACS survey spans avaiable (1-year, 5-year)
#' * endyears: Years of ACS surveys available (for 5-year surveys last year of span)
#'
#' @source \url{https://www2.census.gov/acs2011_1yr/summaryfile/ACS_2011_SF_Tech_Doc.pdf}
"sum_level_info"
