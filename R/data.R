
#' Summary levels and ACS surveys
#'
#' A dataset containing the summary levels supported by [acs_transform()] and the ACS surveys for which they are available.
#'
#' @format A data frame with 8 rows and 4 variables:
#' * geo_type: Geography type
#' * sum_level: Summary level code
#' * spans: ACS survey spans avaiable (1-year, 5-year)
#' * years: Years of ACS surveys available (for 5-year surveys last year of span)
#'
#' @source <https://www2.census.gov/acs2011_1yr/summaryfile/ACS_2011_SF_Tech_Doc.pdf>
"sum_level_info"

#' US State (plus DC and PR) Abbreviations and FIPS codes
#'
#' A dataset containing state fips coed and USPS abbreviations Used internally
#' for downloading files, but also useful to create cevtor of state
#' abbreviations that includes DC and PR.
#'
#' @format A data frame with 53 rows and 3 variables:
#' * abb: Official USPS Code
#' * fips: Fips State Numeric Code
#' * name: State Name (format used for FTP download files)
#'
#' @source <https://www.census.gov/geo/reference/ansi_statetables.html>
"fips_abb_name_table"
