#' Create indicators from clean ACS SF data
#'
#' Import all csv files for a given geography type, endyear, and span that were created by [acs_extract_raw()], and apply a function to each file to further process data from.
#'
#' @param acs_dir \[`character(1)`]: The root directory in which all the ACS SF data will be saved.
#' @param out_dir \[`character(1)`]: The directory to save indicator files to. Defults to "`acs_dir`/Output".
#' @param endyear \[`integer(1)`]: The endyear of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span \[`integer(1)`]: The span of years for ACS estimates. ACS 1-year, and
#'   5-year surveys are supported.
#' @param sum_level \[`character(1)`]: The Census summary level code. For full list of supported summary levels see `sum_level_info`.
#' @param .f A function or formula to be passed to [`purrr::as_mapper()`]. The function must always return a dataframe.
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function. There
#'   are three ways to refer to the arguments:
#'
#'   * For a single argument function, use `.`
#'   * For a two argument function, use `.x` and `.y`
#'   * For more arguments, use `..1`, `..2`, `..3` etc
#'
#'   This syntax allows you to create very compact anonymous functions.
#'
#' @export
#'
acs_make_indicators <- function(acs_dir, out_dir = NULL, endyear, span, sum_level, .f = NULL) {

  if (is.null(.f)) {
    .f <- purrr::as_mapper(~return(.x))
  } else {
    .f <- purrr::as_mapper(.f)
  }

  # TODO: add sum_level to sum_level_name validator helper function
  sum_level_name <- switch(
    sum_level,
    "010" = "us",
    "310" = "cbsa",
    "160" = "place",
    "795" = "puma",
    "040" = "state",
    "050" = "county",
    "140" = "tract",
    "150" = "blockgroup"
  )

  if (is.null(out_dir)) {
    out_dir <- glue("{acs_dir}/Output")
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  out_file <- glue("{out_dir}/{sum_level_name}_{span}_{endyear}_indicators.csv")

  acs_cols <- readr::cols(
    .default = "d",
    endyear = "i",
    span = "i",
    geoid_full = "c",
    geoid = "c",
    sum_level = "c",
    geo_type = "c",
    geo_name = "c"
  )

  # TODO: consider writing to csv after each file, with map_dfr it might get too
  # large for processing blockgroups

  glue("{acs_dir}/Clean/{endyear}_{span}") %>%
    dir(pattern = glue("_{sum_level_name}_"), full.names = TRUE) %>%
    purrr::map_dfr(~{
      .x %>%
        readr::read_csv(col_types = acs_cols) %>%
        .f()
    }) %>%
    readr::write_csv(out_file, na = "")
}
