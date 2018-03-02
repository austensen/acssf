#' Create csv file/dataframe of ACS variables for a given sample and geography
#'
#' This function parses the data downloded by [acs_download()] and creates a csv
#' file for the selected parameters.
#'
#' @param acs_dir \[`character(1)`]: The root directory in which all the ACS
#'   data has been downloaded with [acs_download()].
#' @param endyear \[integer(1)]: The endyear of the desired ACS sample. For
#'   example, use 2010 for the 2010 1-year ACS or the 2006-2010 5-yer ACS.
#' @param span \[integer(1)]: The span of years for ACS estimates. ACS contains
#'   1-, 3-, and 5-year surveys.
#' @param geo \[`charater(1)`]: The 2-letter state abbreviation or 2-digit FIPS
#'   code for the state for which data will be downloaded. For geogrpahies that
#'   do not nest within states, use `"us"`.
#' @param sum_level \[`character(1)`]: The Census Bureau code for the summary
#'   level to include in the table. (eg. "010" = United States) For full list
#'   see <https://factfinder.census.gov/help/en/summary_level_code_list.htm>.
#' @param vars_table \[`data.frame`]: A dataframe with a single column
#'   containing the desired ACS variable codes to be included in the output
#'   table. The ACS codes must be stored as a character column using the format
#'   `"b25003_001"`.
#'
#' @export
#'
acs_extract_raw <- function(acs_dir, endyear, span, geo, sum_level, vars_table) {

  # TODO: check that raw data folder exists

  validate_args(
    endyear = endyear,
    span = span
  )


  # TODO: validate requested sumlevels. Are they available for requested geo type
  # (state or US) and span (1 or 5 year). Add to validators.R file.

  # Might also make a function that returns a table to help people learn the
  # summary level codes. eg. a table with cols: endyear, span, sumlevel code,
  # sumlevel name

  # https://factfinder.census.gov/help/en/summary_level_code_list.htm

  # ACS 1, state
  # state_1_sumlevels <- c("040", "050", "060", "160", "312", "500", "795", "960", "970")
  # us_1_sumlevels <- c("010", "020", "030", "250", "310", "314", "330", "335", "350", "355", "400")
  # state_5_sumlevel
  # us_5_sumlevels

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


  geo_abb <- swap_geo_id(geo, "abb")
  geo_name <- swap_geo_id(geo, "name")

  # capitalize "of" in 5-yr data
  if (span == 5L && geo_abb == "dc") {
    geo_name <- "DistrictOfColumbia"
  }


  trct_blkgrp <- sum_level_name %in% c("tract", "blockgroup")


  raw_dir <- glue("{acs_dir}/Raw/{endyear}_{span}")

  docs_dir <- glue("{raw_dir}/_docs")

  data_dir <- dplyr::case_when(
    span == 1L                 ~ glue_chr("{raw_dir}/{geo_name}"),
    span == 5L && trct_blkgrp  ~ glue_chr("{raw_dir}/{geo_name}/tract_blockgroup"),
    span == 5L && !trct_blkgrp ~ glue_chr("{raw_dir}/{geo_name}/non_tract_blockgroup")
  )

  clean_dir <- glue("{acs_dir}/Clean/{endyear}_{span}")

  dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)


  # TODO: check if data has already been downloaded for the requested params


  # get table of geoid and logrecno to filter other tables
  geos_table_slim <- get_geos_table(
    data_dir = data_dir,
    docs_dir = docs_dir,
    endyear = endyear,
    span = span,
    geo_abb = geo_abb,
    .sum_level = sum_level
  )


  # create a named list of table_vars (names are seq numbers)
  seq_col_lookup <- get_seq_col_lookup(docs_dir, endyear)


  # Iterate over the seq numbers and for each do the following:
  # import that seq number's value files for estimates and margins,
  # filter to just the requested geographies (sumlevels),
  # reshape the files long and combine estiamtes and margins
  # push the final table (for that seq) to the database

  # TODO: see if possible to tweak progress bar to include time spent on
  # reshaping adn writing csv after thir purrr step

  pb <- dplyr::progress_estimated(length(names(seq_col_lookup)))


  values_long <- purrr::map_dfr(
    .x = names(seq_col_lookup),
    .f = import_values,
    seq_col_lookup = seq_col_lookup,
    geos_table = geos_table_slim,
    vars_table = vars_table,
    data_dir = data_dir,
    endyear = endyear,
    span = span,
    geo_abb = geo_abb,
    .pb = pb
  )


  # possible that there are no rows returned (eg. place in WY for 1yr)
  if (nrow(values_long) == 0) return(invisible(NULL))

  if (endyear == 2010L) {
    # There are some tables that appear in multiple Seq files for some reason
    # eg. B05002 is in 23 and 171
    values_long <- dplyr::distinct(values_long)
  }


  values_wide <- values_long %>%
    tidyr::gather("type", "value", estimate, margin) %>%
    dplyr::mutate( # eg. b01001_e001
      table_var = stringr::str_replace(
        table_var,
        "_",
        stringr::str_c("_", stringr::str_sub(type, 1, 1))
      )
    ) %>%
    dplyr::select(-type) %>%
    tidyr::spread(table_var, value) %>%
    dplyr::mutate(geo_type = sum_level_name) %>%
    dplyr::select(
      endyear, span, geoid_full, geoid, sum_level, geo_type, geo_name,
      dplyr::everything()
    )


  readr::write_csv(
    values_wide,
    glue("{clean_dir}/{geo_abb}_{sum_level_name}_{endyear}_{span}.csv"),
    na = ""
  )
}


import_values <- function(seq,
                          seq_col_lookup,
                          geos_table,
                          vars_table,
                          data_dir,
                          endyear,
                          span,
                          geo_abb,
                          .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) {
    .pb$tick()$print()
  }


  # these are always the first columns in the values files
  first_cols <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")

  # get the table_vars for this seq number - these are the last columns
  seq_cols <- seq_col_lookup[[seq]]

  geo_cols <- c("geoid_full", "geoid", "sum_level", "geo_name")

  # all the estimates/margins columns will be read as numeric (double)
  value_cols <- readr::cols(
    .default = "d",
    fileid = "c",
    filetype = "c",
    stusab = "c",
    chariter = "c",
    sequence = "c",
    logrecno = "c"
  )

  estimates <- glue("{data_dir}/e{endyear}{span}{geo_abb}{seq}.txt") %>%
    readr::read_csv(
      col_names = c(first_cols, seq_cols),
      col_types = value_cols,
      na = c("", ".", "..0"),
      progress = FALSE
    )

  # For some geos/seqs the dataset is empty b/c they're for PR specific tables
  if (!length(estimates)) {
    return(tibble::tibble())
  }

  estimates <- estimates %>%
    dplyr::right_join(geos_table, by = "logrecno") %>%
    dplyr::select(geoid, seq_cols) %>%
    tidyr::gather("table_var", "estimate", -geoid)


  margins <- glue("{data_dir}/m{endyear}{span}{geo_abb}{seq}.txt") %>%
    readr::read_csv(
      col_names = c(first_cols, seq_cols),
      col_types = value_cols,
      na = c("", "."),
      progress = FALSE
    ) %>%
    dplyr::right_join(geos_table, by = "logrecno") %>%
    dplyr::select(dplyr::one_of(geo_cols), dplyr::one_of(seq_cols)) %>%
    tidyr::gather("table_var", "margin", -dplyr::one_of(geo_cols))


  # with estimates and margins sorted indentically can bind columns, then join
  # with vars_table to keep only the desired table_vars
  values_long <- estimates %>%
    dplyr::select(-geoid, -table_var) %>%
    dplyr::bind_cols(margins) %>%
    dplyr::semi_join(vars_table, by = "table_var") %>%
    dplyr::mutate(
      endyear = endyear,
      span = span
    )
}
