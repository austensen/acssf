#' Create ACS Table in Database
#'
#' This function parses the data downloded by `acssf::download_acs()` and either creates
#' or appends to a table in a database with American Community Survey (ACS)
#' Summary File (SF) data.
#'
#' @param acs_dir The root directory in which all the ACS SF data will be saved.
#' @param endyear The endyear of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span The span of years for ACS estimates. ACS contains 1-, 3-, and
#'   5-year surveys.
#' @param geo The 2-letter abbreviation for the state for which data will be
#'   downloaded. For geogrpahies that do not nest within states, use `"us"`.
#' @param sumlevels [`character`]: A character vector of the Census Bureau summary levels to include in the table. (eg. "010" = United States) For full list see https://factfinder.census.gov/help/en/summary_level_code_list.htm
#' @param table_name \[`character`]:The name of the table in the database that should be create or appended to.
#' @param conn \[`DBIConnection`]:A DBI connection object obtained from `DBI::dbConnect()`.
#'
#' @export
#'
acs_make_table <- function(acs_dir, endyear, span, geo, sumlevels, table_name, conn) {


  validate_args(
    endyear = endyear,
    span = span
  )


  # TODO: validate requested sumlevels. Are they available for requested geo type
  # (state or US) and span (1 or 5 year). Add to validators.R file.

  # https://factfinder.census.gov/help/en/summary_level_code_list.htm

  # ACS 1, state
  # state_1_sumlevels <- c("040", "050", "060", "160", "312", "500", "795", "960", "970")
  # us_1_sumlevels <- c("010", "020", "030", "250", "310", "314", "330", "335", "350", "355", "400")
  # state_5_sumlevel
  # us_5_sumlevels


  geo_abb <- swap_geo_id(geo, "abb")


  # TODO: check if data is there for the requested params

  raw_dir <- glue("{acs_dir}/Raw/{endyear}_{span}")

  # get table of geoid nad logrecno to filter other tables
  geos_table_slim <- get_geos_table(
    raw_dir = raw_dir,
    endyear = endyear,
    span = span,
    geo_abb = geo_abb,
    sumlevels = sumlevels
  )


  # create a named list of table_vars (names are seq numbers)
  seq_col_lookup <- get_seq_col_lookup(raw_dir, endyear)


  # Iterate over the seq numbers and for each do the following:
  # import that seq number's value files for estimates and margins,
  # filter to just the requested geographies (sumlevels),
  # reshape the files long and combine estiamtes and margins
  # push the final table (for that seq) to the database

  pb <- dplyr::progress_estimated(length(names(seq_col_lookup)))

  purrr::walk(
    .x = names(seq_col_lookup),
    .f = import_values,
    seq_col_lookup = seq_col_lookup,
    geos_table = geos_table_slim,
    raw_dir = raw_dir,
    endyear = endyear,
    span = span,
    geo_abb = geo_abb,
    conn = conn,
    table_name = table_name,
    .pb = pb
  )

}

#' Create Index on ACS Table in Database
#'
#' This function creates a unique index (if it does not exist) on a ACS table
#' in a database created by `acssf::acs_make_table()`.
#'
#' @param conn \[`DBIConnection`]:A DBI connection object obtained from
#'   `DBI::dbConnect()`.
#' @param table_name \[`character`]:The name of the table in the database that
#'   has been created by `acssf::acs_make_table()`.
#'
#' @export
#'
acs_create_indexes <- function(conn, table_name) {

  create_index(conn, table_name, c("endyear", "span", "geoid", "table_var"), "key")
  create_index(conn, table_name, c("geoid", "table_var"), "idx")
  create_index(conn, table_name, "endyear", "idx")
  create_index(conn, table_name, "span", "idx")
  create_index(conn, table_name, "geoid", "idx")
  create_index(conn, table_name, "table_var", "idx")

}

create_index <- function(conn, table, vars, suffix = c("pkey", "key", "idx")) {

  uni <- dplyr::if_else(suffix %in% c("pkey", "key"), "UNIQUE", "")

  DBI::dbSendStatement(conn, glue('
    CREATE {uni} INDEX IF NOT EXISTS
    {table}_{glue::collapse(vars, sep = "_")}_{suffix}
    ON {table} ({glue::collapse(vars, sep = ", ")})
    ')
  )

}


# Geos --------------------------------------------------------------------

# TODO: decide if it's worth creating a geos table in the database. Ideally it
# makes sense, but will it ever really be used if it takes so much storrage to
# keep many years and geographies on the database?

# for now just creating the slim version needed for processing the vars and
# values, and not returning the full version that would be added to the databse

get_geos_table <- function(raw_dir, endyear, span, geo_abb, sumlevels) {

  if (endyear >= 2009) {

    geo_name <- swap_geo_id(geo_abb, "name")

    geo_col_names <- glue("{raw_dir}/_docs/{endyear}_SFGeoFileTemplate.xls") %>%
      readxl::read_excel(n_max = 0) %>%
      names() %>%
      stringr::str_to_lower()

    geos_table_raw <- glue("{raw_dir}/{geo_name}/g{endyear}{span}{geo_abb}.csv") %>%
      readr::read_csv(
        col_names = geo_col_names,
        col_types = readr::cols(.default = "c")
      )

  } else {
    # TODO: add the fwf_cols taken from sas programs (maybe process in /data-raw ?)
    stop_glue("2005 to 2008 not yet available")
  }


  # TODO: decide which columns to keep, for now keep only what we need now
  keep_geo_cols <- c("geoid", "name", "sumlevel", "component", "logrecno", "us", "stusab")


  geos_clean <- geos_table_raw %>%
    dplyr::filter(sumlevel %in% sumlevels) %>%
    dplyr::mutate(
      endyear = endyear,
      span = span
    ) %>%
    dplyr::select(endyear, span, keep_geo_cols)


  # For merging geoid onto values table
  geos_table_slim <- geos_clean %>% dplyr::select(logrecno, geoid)

  # For database
  geos_table <- geos_clean %>% dplyr::select(-logrecno)

  return(geos_table_slim)
}



# Vars --------------------------------------------------------------------


# TODO: decide if its worth having a variable variable lookup table at all. If
# so what should it include and how should it be laid out. Could have one with
# universe, definition, and row descriptions. Is this the best way to look up
# variables?

get_seq_col_lookup <- function(raw_dir, endyear) {

  if (endyear == 2005) {
    # TODO: add code to parse multiple seq tables for seq/vars
    stop_glue("Need to add 2005 functionality")
  } else {
    vars_raw <- glue("{raw_dir}/_docs/seq_table_lookup.xls") %>%
      readxl::read_excel(col_types = "text") %>%
      # column name formats differ, but order is consistent
      dplyr::select(
        table = 2,
        seq = 3,
        line_num = 4
      ) %>%
      # remove extra rows (table fillers) to avoid duplicate table_vars
      dplyr::filter(
        !is.na(line_num),
        !stringr::str_detect(line_num, "\\.")
      ) %>%
      dplyr::transmute(
        # endyear = endyear,
        # span = span,
        table = stringr::str_to_lower(table),
        var = stringr::str_pad(line_num, 3, "left", "0"),
        table_var = stringr::str_c(table, "_", var),
        seq = stringr::str_c(stringr::str_pad(seq, 4, "left", "0"), "000")
      )

    # create a named list of table_vars (names are seq numbers)
    get_seq_col_lookup <- split(vars_raw[["table_var"]], vars_raw[["seq"]])
  }

  return(get_seq_col_lookup)
}


import_values <- function(seq,
                          seq_col_lookup,
                          geos_table,
                          raw_dir,
                          endyear,
                          span,
                          geo_abb,
                          conn,
                          table_name,
                          .pb = NULL) {

  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

  geo_name <- swap_geo_id(geo_abb, "name")

  # these are always the first columns in the values files
  first_cols <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")

  # get the table_vars for this seq number - these are the last columns
  seq_cols <- seq_col_lookup[[seq]]

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

  estimates <- glue("{raw_dir}/{geo_name}/e{endyear}{span}{geo_abb}{seq}.txt") %>%
    readr::read_csv(
      col_names = c(first_cols, seq_cols),
      col_types = value_cols,
      na = c("", "."),
      progress = FALSE
    ) %>%
    dplyr::right_join(geos_table, by = "logrecno") %>%
    dplyr::select(geoid, seq_cols) %>%
    tidyr::gather("table_var", "estimate", -geoid)


  margins <- glue("{raw_dir}/{geo_name}/m{endyear}{span}{geo_abb}{seq}.txt") %>%
    readr::read_csv(
      col_names = c(first_cols, seq_cols),
      col_types = value_cols,
      na = c("", "."),
      progress = FALSE
    ) %>%
    dplyr::right_join(geos_table, by = "logrecno") %>%
    dplyr::select(geoid, seq_cols) %>%
    tidyr::gather("table_var", "margin", -geoid)


  # with estaimates and margins sorted indentically can bind columns
  values_table <- estimates %>%
    dplyr::select(-geoid, -table_var) %>%
    dplyr::bind_cols(margins) %>%
    dplyr::mutate(
      endyear = endyear,
      span = span
    ) %>%
    dplyr::select(endyear, span, geoid, table_var, estimate, margin)


  DBI::dbWriteTable(
    conn,
    name = table_name,
    value = values_table,
    append = TRUE,
    row.names = FALSE
  )

}


