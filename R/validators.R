## swap_geo_id was addapted from tidycensus "validate_state"
## https://github.com/walkerke/tidycensus/blob/12b18d230ba1768f92056919fbab341f3ff63ceb/R/utils.R

#' Return desired geography variable from FIPS or abbreviation
#'
#' @param geo character or numberic, state FIPS code or state (or US) abreviation
#' @param id character, type of geogrphy ID to return ("name", "abb", or "fips")
#'
#' @return Character(1) of either geography name, abbreviation, or FIPS code.
#'

swap_geo_id <- function(geo, id = c("name", "abb", "fips")) {
  geo <- geo %>% stringr::str_trim() %>% stringr::str_to_lower()

  if (geo %in% c("as", "60", "gu", "66", "vi", "78")) {
    stop_glue("Data is not available for American Samoa, Guam, or Virgin Islands.")
  }

  if (stringr::str_detect(geo, "^[[:digit:]]+$")) {
    geo <- stringr::str_pad(geo, 2, "left", "0")

    if (geo %in% fips_abb_name_table[["fips"]]) {
      ret <- fips_abb_name_table %>%
        dplyr::filter(fips == geo) %>%
        dplyr::pull(id)

      return(ret)
    }
  } else if (stringr::str_detect(geo, "^[[:alpha:]]+")) {
    if (nchar(geo) == 2 && geo %in% fips_abb_name_table[["abb"]]) {
      ret <- fips_abb_name_table %>%
        dplyr::filter(abb == geo) %>%
        dplyr::pull(id)

      return(ret)
    }
  }
  stop_glue("{geo} is not a valid FIPS code or abbreviation.")
}



#' Validate function arguments
#'
#' @param endyear \[`integer(1)`]: The endyear of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span \[`integer(1)`]: The span of years for ACS estimates. ACS 1-year, and
#'   5-year surveys are supported.
#' @param overwrite \[`logical(1)`]:  Whether existing versions of these files be overwriten.
#'   Defaults to `FALSE`.
#'
#' @return None
#'

validate_args <- function(endyear, span, overwrite = NULL) {
  endyear <- as.integer(endyear)
  if (is.na(endyear)) {
    stop_glue("`endyear` must be an integer.")
  }

  span <- as.integer(span)
  if (is.na(span)) {
    stop_glue("`span` must be an integer.")
  }

  if (endyear < 2005) {
    stop_glue("ACS is only available from 2005 onwards.")
  }

  if (endyear %in% 2005:2006 && span != 1L) {
    stop_glue("For {endyear} only 1-year data is available.")
  } else if (endyear %in% 2007:2008 & span == 5L) {
    stop_glue("For {endyear} only 1- and 3-year data is available.")
  } else if (endyear >= 2014L && span == 3L) {
    stop_glue("From 2014 onward 3-year data is no longer available.")
  }

  if (!is.null(overwrite) && !is.logical(overwrite)) {
    stop_glue("`overwrite` must be logical.")
  }
}
