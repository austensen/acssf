#' Calculate the a derived sum of ACS variables
#'
#' This function calculates a derived sum of ACS variables from the ACS
#' estimates returned by [acs_transform()]. It assumes the data structure of a
#' dataframe returned by [acs_transform()], where each row is a geography and
#' each column is an ACS estimate or margin of error, and so is used within a
#' [dplyr::mutate()] call and sums across rows. The main argument to this
#' function is first parsed with [acs_vars()] and then the columns are found
#' within the dataframe.
#'
#' @param ...\[`expressions`]: Expressions string(s) to format into ACS variable
#'   estimate codes with [acs_vars()].
#' @param na.rm \[`logical: TRUE`]: A logical value indicating whether NA values
#'   should be stripped before the computation proceeds.
#' @param .envir \[`environment: parent.frame()`]: Environment to evaluate each
#'   expression in. Expressions are evaluated from left to right. If .x is an
#'   environment, the expressions are evaluated in that environment and .envir
#'   is ignored.
#'
#' @export
acs_est_sum <- function(..., na.rm = TRUE, .envir = parent.frame()) {
  list(...) %>%
    purrr::map(acs_vars) %>%
    purrr::flatten_chr() %>%
    purrr::map(~rlang::eval_tidy(rlang::sym(.x), env = .envir)) %>%
    as.data.frame() %>%
    rowSums(na.rm = na.rm)
}

#' Calculate the margin of error for a derived sum of ACS variables
#'
#' This function calculates the margin of error for a derived sum of ACS
#' variables from the ACS estimates and margins of error returned by
#' [acs_transform()]. It assumes the data structure of a dataframe returned by
#' [acs_transform()], where each row is a geography and each column is an ACS
#' estimate or margin of error, and so is used within a [dplyr::mutate()] call
#' and sums across rows. The main argument to this function is first parsed with
#' [acs_vars()] and then the columns are found within the dataframe. The
#' variables provided should be fore the estimates and it will also find the
#' corresponding margins of error columns using the standard naming conversion
#' of [acs_transform()].
#'
#' @inheritParams acs_est_sum
#'
#' @export
acs_moe_sum <- function(..., na.rm = TRUE, .envir = parent.frame()) {

  est_var_names <- list(...) %>%
    purrr::map(acs_vars) %>%
    purrr::flatten_chr()

  # Make sure all the acs_vars given are for the estaimtes
  all_vars_are_estimates <- all(stringr::str_detect(est_var_names, "_e\\d{3}$"))
  stopifnot(all_vars_are_estimates)

  est_df <- est_var_names %>%
    purrr::map(~rlang::eval_tidy(rlang::sym(.x), env = .envir)) %>%
    purrr::set_names(est_var_names) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.data[["id"]]) %>%
    tidyr::pivot_wider(names_from = .data[["id"]]) %>%
    select(-"name")


  moe_var_names <- list(...) %>%
    purrr::map(~stringr::str_replace(., "_e", "_m")) %>%
    purrr::map(acs_vars) %>%
    purrr::flatten_chr()

  moe_df <- moe_var_names %>%
    purrr::map(~rlang::eval_tidy(rlang::sym(.x), env = .envir)) %>%
    purrr::set_names(est_var_names) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.data[["id"]]) %>%
    tidyr::pivot_wider(names_from = .data[["id"]]) %>%
    dplyr::select(-"name")

  # Confirm same number of acs_vars for estimates and margins
  stopifnot(nrow(est_df) == nrow(moe_df))

  purrr::map2(est_df, moe_df, moe_sum, na.rm = na.rm) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    .[["value"]]
}
