#' Download ACS Summary File Data
#'
#' This function downloads American Community Survey (ACS) Summary File (SF)
#' data from the Census Bureau's FTP site.
#'
#' @param year \[`integer(1)`]: The year of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span \[`integer(1)`]: The span of years for ACS estimates. ACS 1-year, and
#'   5-year surveys are supported.
#' @param geo \[`character(1)`]: The 2-letter abbreviation for the state for which data will be
#'   downloaded. For geogrpahies that do not nest within states, use `"US"`.
#' @param acs_dir \[`character(1)`]: The root directory in which all the ACS SF data will be saved. Defaluts to current working directoy.
#' @param overwrite \[`logical(1)`]:  Whether existing versions of these files be overwriten.
#'   Defaults to `FALSE`.
#'
#' @export

acs_download <- function(year, span, geo, acs_dir = ".", overwrite = FALSE) {

  # TODO: add support for taking vector of geos
  # TODO: add progress bar (first time for 5yr in big state takes forever)

  validate_args(
    year = year,
    span = span,
    overwrite = overwrite
  )

  geo_abb <- swap_geo_id(geo, span, "abb")
  geo_name <- swap_geo_id(geo_abb, span, "name")

  docs_dir <- glue("{acs_dir}/Raw/{year}_{span}/_docs")
  data_dir <- glue("{acs_dir}/Raw/{year}_{span}/{geo_name}")

  if (overwrite) {
    fs::dir_delete(c(docs_dir, data_dir))
  } else if (fs::file_exists(data_dir)) {
    warn_glue("{year} {span}-year data for {geo_name} already exists.")
    return(invisible(NULL))
  }

  # some downloads take a long time, temporarily change timeout (10min)
  op <- options(timeout = 600L)
  on.exit(options(op))

  download_docs(docs_dir, year, span)
  download_data(data_dir, year, span, geo_name)

  data_dir %>%
    fs::dir_ls(regexp = ".*\\.zip$", recurse = TRUE) %>%
    fs::file_delete()
}

#' Download Seq/Table/Var Info
#'
#' For a given year/span (eg. 2018 1yr) gets Seq/Table number data and geogrphy
#' IDs data and saves them to the provided local directory for documentation
#' files.
#'
#' @param docs_dir Local path to documentation folder
#' @param year Year of ACS data, as number
#' @param span 1 or 5 year for ACS data, as a number
#'
download_docs <- function(docs_dir, year, span) {

  # Only one set of docs per year/span
  if (fs::dir_exists(docs_dir)) {
    # Don't want to warn because it would come up for every state data is downloaded for
    return(invisible(NULL))
  }

  fs::dir_create(docs_dir, recurse = TRUE)

  base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{year}/documentation")

  # get Seq/Table/Var info
  if (year >= 2006) {
    docs_file_url <- dplyr::case_when(
      year == 2006                ~ glue_chr("{base_url}/merge_5_6_final.xls"),
      year == 2007                ~ glue_chr("{base_url}/{span}_year/merge_5_6_final.xls"),
      year == 2008                ~ glue_chr("{base_url}/{span}_year/user_tools/merge_5_6.xls"),
      year == 2009 && span == 1L  ~ glue_chr("{base_url}/{span}_year/user_tools/merge_5_6.xls"),
      year == 2009 && span == 5L  ~ glue_chr("{base_url}/{span}_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls"),
      year %in% 2010:2012         ~ glue_chr("{base_url}/{span}_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls"),
      year %in% 2013:2017         ~ glue_chr("{base_url}/user_tools/ACS_{span}yr_Seq_Table_Number_Lookup.xls"),
      year == 2018                ~ glue_chr("{base_url}/user_tools/ACS_{span}yr_Seq_Table_Number_Lookup.csv"),
      year >= 2019                ~ glue_chr("{base_url}/user_tools/ACS_{span}yr_Seq_Table_Number_Lookup.txt")
    )

    # standardize when saving local copy for easier lookup later
    docs_file <- glue("{docs_dir}/seq_table_lookup.{fs::path_ext(docs_file_url)}")

    download_files(docs_file_url, docs_file, mode = "wb")
  } else if (year == 2005) {

    # seq/table info
    seq_filename <- "Chapter_5_tables_summary_list.xls"

    seq_url <- glue("{base_url}/{seq_filename}")
    seq_file <- glue("{docs_dir}/{seq_filename}")

    download_files(seq_url, seq_file, mode = "wb")


    # also need table shells to build for table/vars
    shell_base_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2005"
    shell_filenames <- get_filenames_html(shell_base_url, "\\.xls$")

    shell_urls <- glue("{shell_base_url}/{shell_filenames}")
    shell_files <- glue("{docs_dir}/{shell_filenames}")

    download_files(shell_urls, shell_files)
  }

  # for recent years get geography info
  if (year >= 2009) {
    geos_base_url <- dplyr::case_when(
      year <= 2012L ~ glue_chr("{base_url}/{span}_year/geography"),
      year >= 2013L ~ glue_chr("{base_url}/geography")
    )

    geos_filename <- dplyr::case_when(
      span == 5L                     ~ "5_year_Mini_Geo.xlsx",
      span == 1L && year <= 2012L ~ "Mini_Geofile.xls",
      span == 1L && year >= 2013L ~ "1_year_Mini_Geo.xlsx"
    )

    geos_url <- glue_chr("{geos_base_url}/{geos_filename}")

    geos_file <- glue_chr("{docs_dir}/{geos_filename}")

    download_files(geos_url, geos_file)
  }
}


# Downloads Data Tables

download_data <- function(data_dir, year, span, geo_name) {

  # TODO: warn if no files are found on FTP (census error)

  if (fs::dir_exists(data_dir)) {
    warn_glue("The following directory already exists, skipping download\n{data_dir}")
    return(invisible(NULL))
  }

  fs::dir_create(data_dir, recurse = TRUE)

  base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{year}")

  if (year >= 2009) {
    if (span == 1L) {
      data_filenames <- dplyr::case_when(
        year == 2009 ~ glue_chr("{geo_name}.zip"),
        year > 2009  ~ glue_chr("{geo_name}_All_Geographies.zip")
      )

      data_files <- glue("{data_dir}/{data_filenames}")

      data_urls <- glue("{base_url}/data/{span}_year_by_state/{data_filenames}")

      download_files(data_urls, data_files)

    } else if (span == 5L) {
      data_dir_non_tract <- glue("{data_dir}/non_tract_blockgroup")
      data_dir_tract <- glue("{data_dir}/tract_blockgroup")

      fs::dir_create(c(data_dir_non_tract, data_dir_tract))

      data_filenames <- c(
        glue("{geo_name}_All_Geographies_Not_Tracts_Block_Groups.zip"),
        glue("{geo_name}_Tracts_Block_Groups_Only.zip")
      )

      data_files <- c(
        glue("{data_dir_non_tract}/{data_filenames[[1]]}"),
        glue("{data_dir_tract}/{data_filenames[[2]]}")
      )

      data_urls <- glue("{base_url}/data/{span}_year_by_state/{data_filenames}")

      download_files(data_urls, data_files)
    }
  } else if (year <= 2008) {

    # get data files (including geographies)

    data_base_url <- dplyr::case_when(
      year == 2005 & geo_name == "UnitedStates" ~ glue_chr("{base_url}/data/0UnitedStates"),
      year %in% 2005:2006                       ~ glue_chr("{base_url}/data/{geo_name}"),
      year %in% 2007:2008                       ~ glue_chr("{base_url}/data/{span}_year/{geo_name}")
    )

    data_file_pattern <- dplyr::case_when(
      year %in% 2005      ~ "\\.2005-1yr",
      year %in% 2006:2008 ~ glue_chr("{year}{span}")
    )

    data_filenames <- get_filenames_html(data_base_url, data_file_pattern)

    data_urls <- glue("{data_base_url}/{data_filenames}")
    data_files <- glue("{data_dir}/{data_filenames}")

    download_files(data_urls, data_files)
  }

  # Unzip all .zip files into their existing folder, then delete .zip files
  zip_files <- fs::dir_ls(data_dir, regexp = ".*\\.zip$", recurse = TRUE)
  unzip_in_place(zip_files)
  fs::file_delete(zip_files)
}

# Get all files matching pattern from a ACS HTML page
# (ONLY WORKS WITH HTML, NOT FTP)
get_filenames_html <- function(url, pattern = ".*") {

  xml2::read_html(url) %>%
    rvest::html_nodes("table td a") %>%
    rvest::html_attr("href") %>%
    .[-1] %>% # drop first element, always "parent dir"
    stringr::str_subset(pattern)

}

unzip_in_place <- function(path) {
  purrr::walk(path, function(path) {
    utils::unzip(path, exdir = fs::path_dir(path), junkpaths = TRUE)
  })
}

download_files <- function(urls, destfiles, ...) {
  purrr::walk2(urls, destfiles, curl::curl_download, ...)
}
