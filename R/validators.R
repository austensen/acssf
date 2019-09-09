#' Return desired geography variable from FIPS or abbreviation
#'
#' @inheritParams acs_download
#' @param type  \[`character(1)`]: type of geogrphy ID to return ("name", "abb", or "fips")
#'
#' @return Character(1) of either geography name, abbreviation, or FIPS code.
#'
swap_geo_id <- function(geo, span, type = c("name", "abb", "fips")) {
  type <- match.arg(type)

  geo <- geo %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_pad(2, "left", "0")

  if (geo %in% c("as", "60", "gu", "66", "vi", "78")) {
    stop_glue("Data is not available for American Samoa, Guam, or Virgin Islands.")
  }

  # capitalize "of" in 5-yr data
  if (span == 5L && geo %in% c("dc", "11") && type == "name") {
    return("DistrictOfColumbia")
  }

  ret <- fips_abb_name_table %>%
    dplyr::filter_at(c("abb", "fips"), dplyr::any_vars(. == geo)) %>%
    dplyr::pull(type)

  if (length(ret) != 1) {
    stop_glue("Data is not available for geo '{geo}'. Make sure 'geo' has \\
              been provided as either a 2-letter state abbreviation or  \\
              2-digit FIPS state code.")
  }

  ret
}


#' Validate function arguments
#'
#' @param year \[`integer(1)`]: The year of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span \[`integer(1)`]: The span of years for ACS estimates. ACS 1-year, and
#'   5-year surveys are supported.
#' @param overwrite \[`logical(1)`]:  Whether existing versions of these files be overwriten.
#'   Defaults to `FALSE`.
#'
#' @return None
#'
validate_args <- function(year, span, overwrite = NULL, sum_levels = NULL) {
  year <- as.integer(year)
  if (is.na(year)) {
    stop_glue("`year` must be an integer.")
  }

  span <- as.integer(span)
  if (is.na(span)) {
    stop_glue("`span` must be an integer.")
  }

  if (year < 2005) {
    stop_glue("ACS is only available from 2005 onwards.")
  }

  if (year %in% 2005:2006 && span != 1L) {
    stop_glue("For {year} only 1-year data is available.")
  } else if (year %in% 2007:2008 && span == 5L) {
    stop_glue("For {year} only 1- and 3-year data is available.")
  } else if (year >= 2014L && span == 3L) {
    stop_glue("From 2014 onward 3-year data is no longer available.")
  }

  if (!is.null(overwrite) && !is.logical(overwrite)) {
    stop_glue("`overwrite` must be logical.")
  }

  if (!is.null(sum_levels)) {
    bad_sum_levels <- dplyr::setdiff(sum_levels, sum_level_info[["sum_level"]])
    stop_glue("The following values for `sum_levels are invalid:
              {glue::collapse(bad_sum_levels, ',')}
              See acssf::sum_level_info for available sum_levels.")

    if (any(sum_levels %in% c("140", "150")) && span != 5) {
      stop_glue("Tract (140) and Block Group (150) sum levels are only \\
                available for 5-year data.")
    }
  }
}
