#' Parse raw data, calculate new varibles, export as csv
#'
#' This function parses the data downloded by [acs_download()] and creates a csv
#' file for the selected parameters.
#'
#' @param year \[integer(1)]: The year of the desired ACS sample. For
#'   example, use 2010 for the 2010 1-year ACS or the 2006-2010 5-yer ACS.
#' @param span \[integer(1)]: The span of years for ACS estimates. ACS contains
#'   1-, 3-, and 5-year surveys.
#' @param geo \[`charater(1)`]: The 2-letter state abbreviation or 2-digit FIPS
#'   code for the state for which data will be downloaded. For geogrpahies that
#'   do not nest within states, use `"us"`.
#' @param sum_levels \[`character`]: The Census Bureau codes for the summary
#'   level to include in the table. (eg. "010" = United States) For full list
#'   see <https://factfinder.census.gov/help/en/summary_level_code_list.htm>.
#' @param keep_vars \[`character`]: A character vector of ACS variable codes to
#'   be included in the output table, using the format `"b25003_001"`.
#' @param acs_dir \[`character(1)`]: The root directory in which all the ACS
#'   data has been downloaded with [acs_download()]. Defaults to current working directory.
#' @param .f A function or formula to be passed to [`purrr::as_mapper()`]. The
#'   function must take a single dataframe as the only argument, and return a
#'   single dataframe.
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function. There are
#'   three ways to refer to the arguments:
#'
#'   * For a single argument function, use `.` * For a two argument function,
#'   use `.x` and `.y` * For more arguments, use `..1`, `..2`, `..3` etc
#'
#'   This syntax allows you to create very compact anonymous functions.
#'
#' @export
#'
acs_transform <- function(year, span, geo, sum_levels, keep_vars, acs_dir = ".", .f = NULL) {

  validate_args(
    year = year,
    span = span
  )

  if (is.null(.f)) {
    .f <- purrr::as_mapper(~return(.x))
  } else {
    .f <- purrr::as_mapper(.f)
  }

  # TODO: validate requested sumlevels. Are they available for requested geo
  # type (state or US) and span (1 or 5 year). Add to validators.R file. Should
  # also allow sum_level an name or number, and use internal lookup to get the
  # other. If they request something invalid, it should print a list of possible
  # sum_levels as a message

  # Might also make a function that returns a table to help people learn the
  # summary level codes. eg. a table with cols: year, span, sumlevel code,
  # sumlevel name

  # https://factfinder.census.gov/help/en/summary_level_code_list.htm

  # ACS 1, state
  # state_1_sumlevels <- c("040", "050", "060", "160", "312", "500", "795", "960", "970")
  # us_1_sumlevels <- c("010", "020", "030", "250", "310", "314", "330", "335", "350", "355", "400")
  # state_5_sumlevel
  # us_5_sumlevels

  sum_levels <- swap_geo_type(sum_levels, "sum_level")
  geo_types <- swap_geo_type(sum_levels, "geo_type")


  geo_abb <- swap_geo_id(geo, span, "abb")
  geo_name <- swap_geo_id(geo, span, "name")

  trct_blkgrp <- geo_types %in% c("tract", "blockgroup")


  raw_dir <- glue("{acs_dir}/Raw/{year}_{span}")
  docs_dir <- glue("{raw_dir}/_docs")

  data_dir <- dplyr::case_when(
    span == 1L                 ~ glue_chr("{raw_dir}/{geo_name}"),
    span == 5L && trct_blkgrp  ~ glue_chr("{raw_dir}/{geo_name}/tract_blockgroup"),
    span == 5L && !trct_blkgrp ~ glue_chr("{raw_dir}/{geo_name}/non_tract_blockgroup")
  )

  if (!fs::dir_exists(data_dir)) {
    stop_glue("The folllowing folder does not exist.
              {data_dir}
              Make sure to run `acs_download()` for {geo} {year} {span}-year")
  }

  clean_dir <- glue("{acs_dir}/Clean/{year}_{span}")
  fs::dir_create(clean_dir, recursive = TRUE)


  # get table of geoid and logrecno to filter other tables
  geos_table_slim <- get_geos_table(data_dir, docs_dir, year, span, geo_abb, sum_levels)


  # create a named list of table_vars (names are seq numbers), and keep only the
  # ones that contains variables we want to keep
  seq_col_lookup <- get_seq_col_lookup(docs_dir, year) %>%
    purrr::keep(~length(dplyr::intersect(keep_vars, .x)) > 0)


  # Iterate over the seq numbers and for each do the following: import that seq
  # number's value files for estimates and margins, filter to just the requested
  # geographies (sumlevels), combine estiamtes and margins

  pb <- dplyr::progress_estimated(length(seq_col_lookup))


  values <- purrr::map_dfc(
    .x = names(seq_col_lookup),
    .f = import_values,
    seq_col_lookup = seq_col_lookup,
    geos_table = geos_table_slim,
    keep_vars = keep_vars,
    data_dir = data_dir,
    year = year,
    span = span,
    geo_abb = geo_abb,
    .pb = pb
  )


  # Possible that there are no rows returned (eg. place in WY for 1yr)
  if (nrow(values) == 0) return(invisible(NULL))

  # There are some tables that appear in multiple Seq files for some reason
  # eg. B05002 is in 23 and 171, so in the map_dfc() they get a "1" added to
  # the column name and we need to remove these

  first_cols <- c("year", "span", "sum_level", "geoid_full", "geoid", "geo_name")
  acs_var_pat <- "[bc]\\d{5}[a-z]*_[em]\\d{3}"
  keep_acs_vars <- c(
    stringr::str_replace(keep_acs_vars, "_", "_e"),
    stringr::str_replace(keep_acs_vars, "_", "_m")
  )

  values %>%
    dplyr::mutate(
      year = as.integer(year),
      span = as.integer(span)
    ) %>%
    add_na_cols(keep_acs_vars) %>%
    dplyr::select(first_cols, dplyr::matches(acs_var_pat)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(acs_var_pat)), as.double) %>%
    .f() %>% # apply user-provided variable calculation function
    fst::write_fst(glue("{clean_dir}/{geo_abb}_{year}_{span}.fst"))
}


import_values <- function(seq,
                          seq_col_lookup,
                          geos_table,
                          keep_vars,
                          data_dir,
                          year,
                          span,
                          geo_abb,
                          .pb = NULL) {

  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) {
    .pb$tick()$print()
  }

  # These are always the first columns in the values files
  first_cols <- c("fileid", "filetype", "stusab", "character", "sequence", "logrecno")

  # get the acs_vars for this seq number - these are the last columns
  seq_cols <- seq_col_lookup[[seq]]

  seq_keep_vars <- dplyr::intersect(keep_vars, seq_cols)

  geo_cols <- c("geoid_full", "geoid", "sum_level", "geo_name")

  col_names <- c(first_cols, seq_cols)
  col_types <- list(character = 1:6, numeric = 7:length(seq_cols))

  est_file <- dplyr::case_when(
    year == 2005L ~ glue("{data_dir}/{geo_abb}{seq}e.{year}-{span}yr"),
    year >= 2006L ~ glue("{data_dir}/e{year}{span}{geo_abb}{seq}.txt")
  )
  mar_file <- dplyr::case_when(
    year == 2005L ~ glue("{data_dir}/{geo_abb}{seq}m.{year}-{span}yr"),
    year >= 2006L ~ glue("{data_dir}/m{year}{span}{geo_abb}{seq}.txt")
  )

  estimates <- read_acs_csv(est_file, col_names, col_types)

  # For some geos/seqs the dataset is empty b/c they're for PR specific tables
  if (!length(estimates)) {
    return(tibble::tibble())
  }

  estimates <- estimates %>%
    dplyr::select("logrecno", seq_keep_vars) %>%
    dplyr::rename_at(seq_keep_vars, stringr::str_replace, pattern = "(.*)(\\d{3})$", replacement = "\\1e\\2")

  margins <- read_acs_csv(mar_file, col_names, col_types) %>%
    dplyr::select_at(seq_keep_vars, stringr::str_replace, pattern = "(.*)(\\d{3})$", replacement = "\\1m\\2")

  # with estimates and margins sorted indentically can bind columns instead of join
  estimates %>%
    dplyr::bind_cols(margins) %>%
    dplyr::right_join(geos_table, by = "logrecno") %>%
    dplyr::select(-"logrecno") %>%
    dplyr::select(geo_cols, dplyr::everything()) %>%
    dplyr::as_tibble()
}

read_acs_csv <- function(file, col_names, col_types) {

  suppressWarnings( # warns for length=0 files, but this is handled above
    data.table::fread(
      file = file,
      sep = ",",
      header = FALSE,
      col.names = col_names,
      colClasses = col_types,
      na.strings = c("", ".", "..0"),
      stringsAsFactors = FALSE,
      data.table = FALSE,
      showProgress = FALSE
    )
  )
}

# Not all variables will be present in all years of data, so to make sure all
# the files are consistent when the user's variable creation function is applied
# all their requested variables are added t the dataframe with NA if they
# weren't in the data.
add_na_cols <- function(.data, col_names) {
  new_cols <- purrr::discard(col_names, ~.x %in% names(.data))

  new_cols_df <- new_cols %>%
    purrr::map(~rep(NA_real_, nrow(.data))) %>%
    purrr::set_names(new_cols)

  dplyr::bind_cols(.data, new_cols_df)
}

