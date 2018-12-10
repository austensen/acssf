
parse_acs_vars <- function(file) {
  file <- readr::read_file(file)

  # get all ACS variables from the R script using the reguular expression pattern
  # looks for B and C tables of the formats "b01001_e002" or "B01001A_e001"

  single_vars <- file %>%
    stringr::str_to_lower() %>%
    stringr::str_extract_all("[bc]\\d+[a-z]*_[em]\\d{3}") %>%
    purrr::flatten_chr() %>%
    unique()


  # Sometimes in the R script we use acssf::acs_sum() to get sequences of
  # variables, eg. acs_sum("b01001_e{c(1:3, 5, 8)*})

  # The above code will miss these, so below we look for lines that contain
  # "acs_sum", extract the string inputs, and evaluate the string versions of this
  # code with acssf::acs_vars() to get the full set of variable names.

  sequence_vars <- file %>%
    stringr::str_extract_all('\\"[bc].*\\"') %>%
    purrr::flatten_chr() %>%
    stringr::str_replace_all('\\"', '') %>%
    unique() %>%
    purrr::map(~{
      glue::glue('acs_vars("{.x}")') %>%
        rlang::parse_expr() %>%
        rlang::eval_tidy()
    }) %>%
    purrr::flatten_chr() %>%
    # fix problems from expressions like: acs_sum("b01001_e{1:2}", "b01001_e{4:5}")
    purrr::map(stringr::str_split, pattern = ",") %>%
    purrr::flatten() %>%
    purrr::flatten_chr() %>%
    stringr::str_trim() %>%
    unique()


  # Combine both sets of variables, change the format for use in acssf processing
  c(single_vars, sequence_vars) %>%
    stringr::str_replace("_[em]", "_") %>%
    stringr::str_sort() %>%
    unique()
}
