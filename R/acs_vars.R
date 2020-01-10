#' Create ACS variable codes
#'
#' Use [glue::glue()] syntax to format and interpolate strings into ACS variable
#' codes. By placing * at the end of a glue expression the values will be padded
#' with `"0"` to length 3.
#'
#' @param ... \[`expressions`]: Expressions string(s) to format, multiple inputs
#'   are concatenated together before formatting.
#' @param .envir \[`environment: parent.frame()`]: Environment to evaluate each
#'   expression in. Expressions are evaluated from left to right. If .x is an
#'   environment, the expressions are evaluated in that environment and .envir
#'   is ignored.
#'
#' @seealso <http://glue.tidyverse.org/articles/transformers.html> for
#'   explanation of transformers used here to pad numbers.
#'
#' @examples
#' acs_vars("b25003_e{1:3*}")
#'
#' acs_vars("b25003_e{c(1, 3)*}")
#'
#' @export
acs_vars <- function(..., .envir = parent.frame()) {
  glue::glue(..., .envir = .envir, .transformer = pad_transformer)
}

pad_transformer <- function(code, envir) {
  if (stringr::str_detect(code, "[*]$")) {
    code <- stringr::str_replace(code, "[*]$", "")
    res <- glue::identity_transformer(code, envir)
    stringr::str_pad(res, 3, "left", "0")
  } else {
    glue::identity_transformer(code, envir)
  }
}

#' Drop ACS variable columns from dataframe
#'
#' Simple wrapper of [dplyr::select()] to drop ACS variable columns (estimates
#' or margins) created by [acs_transform()].
#'
#' @param .data \[`data.frame`]: A data.frame with ACS variable columns using
#'   the format created by [acs_transform()]: `b25002_e001` for estimates and
#'   `b25002_m001` for margins.
#' @param type \[character(1)]: The type of columns to drop. Must be one of:
#'   `"all"`, `"estimates"`, `"margins"`, or `"standard errors"`
#' @details This function simply wraps the `dplyr` expression `select(df,
#'   -matches("pattern"))`, where the regular expression is
#'   `"[bc]\\d{5}[a-z]*_e\\d{3}"` for estimates and `"[bc]\\d{5}[a-z]*_m\\d{3}"`
#'   for margins.
#'
#' @examples
#'
#' library(dplyr)
#'
#' df <- tibble(
#'   year = 2009L,
#'   span = 1L,
#'   b25070_e001 = c(1, 2, 3),
#'   b25070_m001 = c(0.2, 0.5, 0.7),
#'   b25070_e002 = c(3, NA, 1),
#'   b25070_m002 = c(0.1, NA, 0.4)
#' )
#'
#' acs_drop_vars(df, "estimates")
#'
#' acs_drop_vars(df, "margins")
#'
#' df %>%
#'   mutate(foo = acs_est_sum("b25070_e{1:2*}")) %>%
#'   acs_drop_vars("all")
#'
#' @export
acs_drop_vars <- function(.data, type = c("all", "estimates", "margins", "standard errors")) {
  type <- match.arg(type)
  type_pat <- switch(
    type,
    "all" = "([em]|(se))",
    "estimates" = "e",
    "margins" = "m",
    "standard errors" = "se"
  )
  dplyr::select(.data, -dplyr::matches(glue("[bc]\\d{{5}}[a-z]*_{type_pat}\\d{{3}}")))
}


#' Convert All Margin of Error columns to Standard Errors
#'
#' By default ACS data has columns for estimates and their associated margins of
#' error (at the 90% level). When calculating new variables from ACS estiamtes
#' it's often easier to work with standard errors rather than margins of error.
#' This function will convert every margin of error column (eg. `b25064_m001`)
#' into standard errors for a given confidence level (eg. `b25064_se001`). Once
#' this conversion has been made there are a variety of [standard error
#' calculation helpers][acs_se_sum()] available in this package.
#'
#' @param .data Dataframe as created by [acs_transform()] before any changes
#'   have been made.
#' @param level Confidence level for standard errors. Defaults to 0.90.
#'
acs_margin_to_se <- function(.data, level = 0.90) {
  if (level <= 0 || level >= 1) {
    stop_glue("level must be betwen 0 and 1")
  }

  z_score <- stats::qnorm((1.0-level)/2, lower.tail = F)
  z_score_90 <- stats::qnorm((1.0-0.90)/2, lower.tail = F)

  .data %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("[bc]\\d{5}[a-z]*_m\\d{3}")),
      dplyr::funs((z_score * (. / z_score_90))/z_score)
    ) %>%
    dplyr::rename_at(
      dplyr::vars(dplyr::matches("[bc]\\d{5}[a-z]*_m\\d{3}")),
      dplyr::funs(stringr::str_replace(., "_m", "_se"))
    )
}
