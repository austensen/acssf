
acs_dir <- "/Users/Maxwell/acssf"
endyear <- 2016L
span <- 1L
geo <- "US"
overwrite <- FALSE

# sumlevels <- c("160", "795", "050", "140")
sumlevels <- c("010", "310")


source("R/utils.R")
source("R/validators.R")
load("R/sysdata.rda")

`%>%` <- dplyr::`%>%`
glue <- glue::glue



# create the three tables
# vars:
# seq lookup (2009-2016), merge56 (2006-2009), seq tables (2005)
# endyear, span, table, var, table_var, description, universe

# values
# endyear, span, geoid, table_var, estiamte, margin

# geos:
# templates and delimtied geo.txt (2009-2016), col_pos and fwf geo.txt
# endyear, span, geoid, sumlevel, name, ...?



# make_tables() -----------------------------------------------------------


validate_args(
  endyear = endyear,
  span = span,
  overwrite = overwrite
)


# TODO: validate requested sumlevels. Are they available for requested geo type
# (state or US) and span (1 or 5 year). Add to validators.R file.

# https://factfinder.census.gov/help/en/summary_level_code_list.htm

# ACS 1, state
# state_1_sumlevels <- c("040", "050", "060", "160", "312", "500", "795", "960", "970")
# us_1_sumlevels <- c("010", "020", "030", "250", "310", "314", "330", "335", "350", "355", "400")
# state_5_sumlevel
# us_5_sumlevels

geo_name <- swap_geo_id(geo, "name")
geo_abb <- swap_geo_id(geo, "abb")


raw_dir <- glue("{acs_dir}/Raw/{endyear}_{span}")




# Geos --------------------------------------------------------------------


if (endyear >= 2009) {

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




# Vars --------------------------------------------------------------------


# TODO: decide if its worth having a variable variable lookup table at all. If
# so what should it include and how should it be laid out. Could have one with
# universe, definition, and row descriptions. Is this the best way to look up
# variables?


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
    endyear = endyear,
    span = span,
    table = stringr::str_to_lower(table),
    var = stringr::str_pad(line_num, 3, "left", "0"),
    table_var = stringr::str_c(table, "_", var),
    seq = stringr::str_c(stringr::str_pad(seq, 4, "left", "0"), "000")
  )


# Values ------------------------------------------------------------------

# The "!!type" is just a way to tell R to use the value of that object
# rather than the name of the object. So if we are processing the estimates
# file it will create column called "estimate" not "type"

import_values <- function(seq, type) {

  # these are always the first columns in the values files
  first_cols <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")

  # get the table_vars for this seq number - these are the last columns
  seq_cols <- seq_col_names[[seq]]

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

  type_letter <- stringr::str_sub(type, 1, 1)

  df <- glue("{raw_dir}/{geo_name}/{type_letter}{endyear}{span}{geo_abb}{seq}.txt") %>%
    readr::read_csv(
      col_names = c(first_cols, seq_cols),
      col_types = value_cols,
      na = c("", ".")
    ) %>%
    dplyr::right_join(geos_table_slim, by = "logrecno") %>%
    dplyr::select(geoid, seq_cols) %>%
    tidyr::gather("table_var", !!type, -geoid)
}

# create a named list of table_vars (names are seq numbers)
seq_col_names <- split(vars_raw[["table_var"]], vars_raw[["seq"]])

# TODO: Too much data to import all seq files into in-memory dataframe (eg. for
# all US and all MSAs in one year it's >16million rows). Will need to insert
# data to database as part of the iteration.

# Iterate over the seq numbers, importing that seq numbers values file, and
# stacking everything together at the end, for estaimtes and margins

estimates <- purrr::map_df(names(seq_col_names), import_values, type = "estimate")

margins <- purrr::map_df(names(seq_col_names), import_values, type = "margin")


# with estaimates and margins sorted indentically can bind columns
values_table <- estimates %>%
  dplyr::select(-geoid, -table_var) %>%
  dplyr::bind_cols(margins) %>%
  dplyr::mutate(
    endyear = endyear,
    span = span
  ) %>%
  dplyr::select(endyear, span, geoid, table_var, estimate, margin)



