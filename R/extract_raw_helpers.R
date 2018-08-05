



# TODO: decide if its worth having a variable variable lookup table at all. If
# so what should it include and how should it be laid out. Could have one with
# universe, definition, and row descriptions. Is this the best way to look up
# variables?


# Vars --------------------------------------------------------------------


get_seq_col_lookup <- function(docs_dir, endyear) {
  if (file.exists(glue("{docs_dir}/seq_col_lookup.rds"))) {
    readr::read_rds(glue("{docs_dir}/seq_col_lookup.rds"))
  } else {
    make_seq_col_lookup(docs_dir, endyear)
  }
}


make_seq_col_lookup <- function(docs_dir, endyear) {
  if (endyear == 2005) {
    # TODO: add code to parse multiple seq tables for seq/vars
    stop_glue("Need to add 2005 functionality")
  } else {
    vars_raw <- glue("{docs_dir}/seq_table_lookup.xls") %>%
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
        line_num != "0",
        !stringr::str_detect(line_num, "\\.")
      ) %>%
      dplyr::transmute(
        table = stringr::str_to_lower(table),
        var = stringr::str_pad(line_num, 3, "left", "0"),
        table_var = stringr::str_c(table, "_", var),
        seq = stringr::str_c(stringr::str_pad(seq, 4, "left", "0"), "000")
      )

    # create a named list of table_vars (names are seq numbers)
    seq_col_lookup <- split(vars_raw[["table_var"]], vars_raw[["seq"]])
  }


  readr::write_rds(seq_col_lookup, glue("{docs_dir}/seq_col_lookup.rds"))
}



# Geos --------------------------------------------------------------------

get_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb, .sum_level) {

  # TODO: once al bugs fixed, check if file exists before making

  # if (file.exists(glue("{data_dir}/geos_table.rds"))) {
  #
  #   geos_table <- readr::read_rds(glue("{data_dir}/geos_table.rds"))
  #
  # } else {

  geos_table <- make_geos_table(
    data_dir = data_dir,
    docs_dir = docs_dir,
    endyear = endyear,
    span = span,
    geo_abb = geo_abb
  )
  # }

  dplyr::filter(geos_table, sum_level == .sum_level)
}


make_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb) {
  if (span == 5L) {
    if (endyear >= 2016) {
      geos_table_raw <- glue("{docs_dir}/{geo_abb}.xlsx") %>%
        readxl::read_xlsx(col_types = "text", skip = 1) %>%
        dplyr::select(2:4) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))
    } else if (endyear <= 2015) {
      geos_table_raw <- glue("{docs_dir}/{geo_abb}.xls") %>%
        readxl::read_xls(col_types = "text", skip = 1) %>%
        dplyr::select(2:4) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))
    }
  } else if (span == 1L) {
    if (endyear <= 2008L) {

      # no template files in these years
      # so need to get col names and positions from sas programs
      geo_fwf_cols <- get_geo_fwf_cols(endyear)

      geos_table_raw <- readr::read_fwf(
        glue("{data_dir}/g{endyear}{span}{geo_abb}.txt"),
        col_positions = geo_fwf_cols,
        col_types = readr::cols(.default = "c")
      )
    } else if (endyear <= 2012L) {
      geos_filename <- dplyr::case_when(
        endyear <= 2012L ~ "Mini_Geofile.xls",
        endyear == 2013L ~ "1_year_Mini_Geo.xls"
      )

      geo_abb <- dplyr::case_when(
        endyear <= 2010L       ~ stringr::str_to_upper(geo_abb),
        endyear %in% 2011:2012 ~ geo_abb
      )

      geos_table_raw <- glue("{docs_dir}/{geos_filename}") %>%
        readxl::read_xls(sheet = geo_abb, col_types = "text", skip = 1) %>%
        dplyr::select(1:3) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))
    } else if (endyear >= 2013L) {
      geos_filename <- dplyr::case_when(
        endyear == 2013L ~ "1_year_Mini_Geo.xls", # actually an xlsx file, saved as ".xls"
        endyear >= 2014L ~ "1_year_Mini_Geo.xlsx"
      )

      geo_abb <- dplyr::case_when(
        endyear == 2013L ~ stringr::str_to_upper(geo_abb),
        endyear == 2014L ~ geo_abb,
        endyear >= 2015L ~ stringr::str_to_upper(geo_abb)
      )

      geos_table_raw <- glue("{docs_dir}/{geos_filename}") %>%
        readxl::read_xlsx(sheet = geo_abb, col_types = "text", skip = 1) %>%
        dplyr::select(1:3) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))
    }
  }

  geos_table_raw %>%
    dplyr::mutate(
      sum_level = stringr::str_sub(geoid_full, 1, 3),
      geoid = stringr::str_extract(geoid_full, "\\d+$")
    ) %>%
    readr::write_rds(glue("{data_dir}/geos_table.rds"))
}



get_geo_fwf_cols <- function(endyear) {
  if (endyear == 2005L) {
    # taken from:
    # https://www2.census.gov/programs-surveys/acs/summary_file/2005/documentation/0sas_exampleprograms/acssfgeo.sas

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(111, 150),
      geo_name = c(151, NA)
    )
  } else if (endyear %in% 2006:2007) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2007/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    # are no files for 2006, so use 2005 positions

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  } else if (endyear == 2008L) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2008/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  }
}
