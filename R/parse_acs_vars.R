#' Parse and R script for ACS Variables Used
#'
#' This function parses an R script to extract all the ACS variables that are
#' used within it. These can then be passed to the `keep_vars` argument of
#' [acs_transform()]. This helps limit the data that is extracted from raw files
#' and processed to only what's needed to create your derived variables.
#'
#' @param file Path to one or more R scripts used to create your derived variables from
#'   ACS-SF data.
#'   Should include ACS variables using the format `b01001_e001`
#'   for estimates, `b01001_m001` for margins, and `b01001_se001` for standard
#'   errors. Use of [acs_est_sum()] to specify variables will also be correctly
#'   parsed.
#'
#' @export

parse_acs_vars <- function(file) {
  code <- file %>%
    purrr::map_chr(readr::read_file) %>%
    stringr::str_c(collapse = "\n")


  # get all ACS variables from the R script using the reguular expression pattern
  # looks for B and C tables of the formats "b01001_e002" or "B01001A_e001"

  single_vars <- code %>%
    stringr::str_to_lower() %>%
    stringr::str_extract_all("[bc]\\d+[a-z]*_([em]|(se))\\d{3}") %>%
    purrr::flatten_chr() %>%
    unique()


  # Sometimes in the R script we use acssf::acs_est_sum() to get sequences of
  # variables, eg. acs_est_sum("b01001_e{c(1:3, 5, 8)*})

  # The above code will miss these, so below we look for lines that contain
  # "acs_est_sum", extract the string inputs, and evaluate the string versions of this
  # code with acssf::acs_vars() to get the full set of variable names.

  sequence_vars <- code %>%
    stringr::str_extract_all('\\"[bc].*?\\"') %>%
    purrr::flatten_chr() %>%
    stringr::str_replace_all('\\"', '') %>%
    unique() %>%
    purrr::map(~{
      glue::glue('acs_vars("{.x}")') %>%
        rlang::parse_expr() %>%
        rlang::eval_tidy()
    }) %>%
    purrr::flatten_chr() %>%
    # fix problems from expressions like: acs_est_sum("b01001_e{1:2*}", "b01001_e{4:5*}")
    purrr::map(stringr::str_split, pattern = ",") %>%
    purrr::flatten() %>%
    purrr::flatten_chr() %>%
    stringr::str_trim() %>%
    unique()


  # Combine both sets of variables, change the format for use in acssf processing
  c(single_vars, sequence_vars) %>%
    stringr::str_replace("_([em]|(se))", "_") %>%
    stringr::str_sort() %>%
    unique()
}
