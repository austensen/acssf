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

#' Sum rows within mutate with [acs_vars()]
#'
#' Sum across rows within a [dplyr::mutate()] call using [acs_vars()] syntax to
#' express column inputs as expression strings.
#'
#' @param ... \[`expressions`]: Expressions string(s) to format into ACS
#'   variable codes with [acs_vars()] or strings corresponding to column names.
#'   are concatenated together before formatting.
#' @param na.rm \[`logical: TRUE`]: A logical value indicating whether NA values
#'   should be stripped before the computation proceeds.
#' @param .envir \[`environment: parent.frame()`]: Environment to evaluate each
#'   expression in. Expressions are evaluated from left to right. If .x is an
#'   environment, the expressions are evaluated in that environment and .envir
#'   is ignored.
#'
#' @seealso [acs_vars()]
#'
#' @examples
#'
#' library(dplyr)
#'
#' df <- tibble(
#'   b25070_e001 = c(1, 2, 3),
#'   b25070_e002 = c(3, NA, 1),
#'   b25070_e003 = c(1, 2, NA)
#' )
#'
#' df %>%
#'   mutate(
#'     foo = acs_sum("b25070_e{1:2*}"),
#'     bar = acs_sum("b25070_e{1:2*}", "b25070_e003"),
#'     baz = acs_sum("b25070_e002", "b25070_e003")
#'   )
#'
#' @export
acs_sum <- function(..., na.rm = TRUE, .envir = parent.frame()) {
  df <- acs_vars_to_df(..., .envir = .envir)
  rowSums(df, na.rm = na.rm)
}

#' Drop ACS variable columns from dataframe
#'
#' Simple wrapper of [dplyr::select()] to drop ACS variable columns (estimates
#' or margins) created by [acssf::make_raw_table()].
#'
#' @param df \[`data.frame`]: A data.frame with ACS variable columns using the
#'   format created by [acs_make_table()]: `b25002_e001` for estimates and
#'   `b25002_m001` for margins.
#'
#' @details This function simply wraps the `dplyr` expression `select(df,
#'   -maches("pattern"))`, where the regular expression is
#'   `"[bc]\\d{5}[a-z]*_e\\d{3}"` for estiamtes and `"[bc]\\d{5}[a-z]*_m\\d{3}"`
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
#'   mutate(foo = acs_sum("b25070_e{1:2*}")) %>%
#'   acs_drop_vars("both")
#'
#' @name acs_drop
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


acs_margin_to_se <- function(.data, level = 0.90) {
  if (level <= 0 || level >= 1) stop_glue("level must be betwen 0 and 1")

  z_score <- qnorm((1.0-level)/2, lower.tail = F)
  z_score_90 <- qnorm((1.0-0.95)/2, lower.tail = F)

  .data %>%
    dplyr::mutate_at(
      dplyr::maches("[bc]\\d{5}[a-z]*_m\\d{3}"),
      dplyr::funs(z_score * (. / z_score_90))
    ) %>%
    dplyr::rename_at(
      dplyr::maches("[bc]\\d{5}[a-z]*_m\\d{3}"),
      dplyr::funs(stringr::str_replace(., "_m", "_se"))
    )
}



# Standard Error of Calculated Indicator Functions
#################################

# TODO: Test these further then document and export

# Sum or Difference of Estimates
acs_se_sum <- function(..., na.rm = TRUE, .envir = parent.frame()) {
  df <- acs_vars_to_df(..., .envir = .envir)

  df %>%
    dplyr::mutate_all(dplyr::funs(.^2)) %>%
    rowSums(na.rm = na.rm) %>%
    sqrt(.) # raises "note" if input piped in without the dot
}

# Proportions
acs_se_prop <- function(num, num_se, denom, denom_se) {
  purrr::pmap_dbl(list(num, num_se, denom, denom_se), se_prop)
}

se_prop <- function(num, num_se, denom, denom_se) {
  p <- num / denom

  if (p == 1) {
    num_se / denom
  } else if (p < 0) {
    (1 / denom) * sqrt(num_se^2 + p^2 * denom_se^2)
  } else {
    (1 / denom) * sqrt(num_se^2 - p^2 * denom_se^2)
  }
}

# Means and Other Ratios
acs_se_ratio <- function(num, num_se, denom, denom_se) {
  p <- num / denom
  (1 / denom) * sqrt(num_se^2 + p^2 * denom_se^2)
}

# Products
acs_se_prod <- function (val_1, se_1, val_2, se_2) {
  sqrt(val_1 * se_2^2 + val_2 * se_1^2)
}



acs_vars_to_df <- function(..., .envir = parent.frame()) {
  list(...) %>%
    purrr::map(acs_vars) %>%
    purrr::flatten_chr() %>%
    purrr::map(~rlang::eval_tidy(rlang::sym(.x), env = .envir)) %>%
    as.data.frame()
}
