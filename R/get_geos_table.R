

get_geos_table <- function(data_dir, docs_dir, year, span, geo_abb, .sum_levels) {

  if (fs::file_exists(glue("{data_dir}/geos_table.rds"))) {

    geos_table <- readr::read_rds(glue("{data_dir}/geos_table.rds"))

  } else {

    geos_table <- make_geos_table(
      data_dir = data_dir,
      docs_dir = docs_dir,
      year = year,
      span = span,
      geo_abb = geo_abb
    )
  }

  dplyr::filter(geos_table, sum_level %in% .sum_levels)
}


make_geos_table <- function(data_dir, docs_dir, year, span, geo_abb) {

  if (span == 1L && year <= 2008L) {

    geos_filename <- dplyr::case_when(
      year == 2005L ~ glue("{data_dir}/{geo_abb}geo.2005-1yr"),
      year >= 2006L ~ glue("{data_dir}/g{year}{span}{geo_abb}.txt")
    )

    # no template files in these years, so need to get col names and positions
    # from sas programs provided by Census
    geo_fwf_cols <- get_geo_fwf_cols(year)

    geos_table_raw <- readr::read_fwf(
      geos_filename,
      col_positions = geo_fwf_cols,
      col_types = readr::cols(.default = "c")
    )
  } else {
    excel_geo_file <- dplyr::case_when(
      span == 1L && year <= 2012L ~ glue("{docs_dir}/Mini_Geofile.xls"),
      TRUE                        ~ glue("{docs_dir}/{span}_year_Mini_Geo.xlsx")
    )

    keep_cols <- dplyr::case_when(
      span == 1L && year <= 2016L ~ 1:3,
      TRUE                        ~ 2:4 # has a "state" column at beginning
    )

    geo_abb <- match_sheet_case(geo_abb, excel_geo_file)

    geos_table_raw <- excel_geo_file %>%
      readxl::read_excel(sheet = geo_abb, col_types = "text") %>%
      dplyr::select(keep_cols) %>%
      purrr::set_names(c("logrecno", "geoid_full", "geo_name"))
  }

  geos_table_raw %>%
    dplyr::mutate(
      sum_level = stringr::str_sub(geoid_full, 1, 3),
      geoid = stringr::str_extract(geoid_full, "\\d+$")
    ) %>%
    readr::write_rds(glue("{data_dir}/geos_table.rds"))
}

# The case of the excel sheet names change all the time. This checks them and
# converts the provided geo_abb accordingly
match_sheet_case <- function(geo_abb, excel_file) {

  sheet_names <- readxl::excel_sheets(excel_file)

  if (stringr::str_detect(sheet_names[[1]], "[A-Z]")) {
    stringr::str_to_upper(geo_abb)
  } else {
    stringr::str_to_lower(geo_abb)
  }
}


get_geo_fwf_cols <- function(year) {
  if (year == 2005L) {
    # taken from:
    # https://www2.census.gov/programs-surveys/acs/summary_file/2005/documentation/0sas_exampleprograms/acssfgeo.sas

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(111, 150),
      geo_name = c(151, NA)
    )
  } else if (year %in% 2006:2007) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2007/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    # are no files for 2006, so use 2007 positions

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  } else if (year == 2008L) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2008/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    readr::fwf_cols(
      logrecno = c(14, 20),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  }
}
