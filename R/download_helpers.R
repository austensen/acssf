

# scrape census ftp webpage for list of files to download
scrape_filenames <- function(url, pattern = ".*") {
  xml2::read_html(url) %>%
    rvest::html_nodes("table td a") %>%
    rvest::html_attr("href") %>%
    .[-1] %>% # first always "parent dir"
    purrr::keep(stringr::str_detect, pattern = pattern)
}

# preset arguments for downloding multiple files with "libcurl"
# split url/file lists to downlod in groups
download_files <- function(urls, destfiles, group_size = 5, method = "libcurl", quiet = TRUE, ...) {

  # preset options for download.file
  download <- purrr::partial(utils::download.file, method = "libcurl", quiet = TRUE, ...)

  if (length(urls) <= group_size) {
    download(urls, destfiles)
  } else {
    # break list of files into groups when geting many files
    groups <- ceiling(seq_along(urls) / group_size)

    urls <- split(urls, groups)
    destfiles <- split(destfiles, groups)

    purrr::walk2(urls, destfiles, ~{
      download(.x, .y)
      # add delays to be kind to servers
      Sys.sleep(sample(c(1, 2, 5), 1))
    })
  }
}


# wrap unzip in walk to allow for unzipping multiple files, and preset arguments
unzip_files <- function(zip_files, exdir, files = NULL, junkpaths = TRUE, ...) {
  purrr::walk(zip_files, utils::unzip, exdir = exdir, junkpaths = TRUE, ...)
}

# Downloads Seq/Table/Var Info
download_docs <- function(doc_dir, endyear, span) {

  if (!file.exists(doc_dir)) {

    dir.create(doc_dir, showWarnings = FALSE)

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    docs_base_url <- glue("{base_url}/documentation")

    # get Seq/Table/Var info
    if (endyear >= 2006) {

      docs_file_url <- dplyr::case_when(
        endyear == 2006        ~ as.character(glue("{docs_base_url}/merge_5_6_final.xls")),
        endyear == 2007        ~ as.character(glue("{docs_base_url}/{span}_year/merge_5_6_final.xls")),
        endyear %in% 2008:2009 ~ as.character(glue("{docs_base_url}/{span}_year/user_tools/merge_5_6.xls")),
        endyear %in% 2010:2012 ~ as.character(glue("{docs_base_url}/{span}_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls")),
        endyear >=2013         ~ as.character(glue("{docs_base_url}/user_tools/ACS_{span}yr_Seq_Table_Number_Lookup.xls"))
      )

      # standardize when saving local copy for easier lookup later
      docs_file <- glue("{doc_dir}/seq_table_lookup.xls")

      download_files(docs_file_url, docs_file, mode = "wb")

    } else if (endyear == 2005) {

      # seq/table info
      seq_filename <- "Chapter_5_tables_summary_list.xls"

      seq_url <- glue("{docs_base_url}/{seq_filename}")
      seq_file <- glue("{doc_dir}/{seq_filename}")

      download_files(seq_url, seq_file, mode = "wb")


      # also need table shells to build for table/vars
      shell_base_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2005"
      shell_filenames <- scrape_filenames(shell_base_url, "\\.xls$")

      shell_urls <- glue("{shell_base_url}/{shell_filenames}")
      shell_files <- glue("{doc_dir}/{shell_filenames}")

      download_files(shell_urls, shell_files)
    }

    # for recent years get geography columns info
    if (endyear >= 2009) {

      # File templates (for geography cols)
      templates_filename <- ifelse(
        endyear == 2010,
        glue("{endyear}_{span}yr_SummaryFileTemplates.zip"),
        glue("{endyear}_{span}yr_Summary_FileTemplates.zip")
      )

      templates_url <- glue("{base_url}/data/{templates_filename}")
      templates_file <- glue("{doc_dir}/{templates_filename}")

      download_files(templates_url, templates_file)

      # unzip just the geography file
      zipped_geo_file <- utils::unzip(templates_file, list = TRUE) %>%
        dplyr::pull("Name") %>%
        purrr::keep(stringr::str_detect, pattern = "SFGeoFileTemplate\\.xls$")

      utils::unzip(templates_file, files = zipped_geo_file, exdir = doc_dir, junkpaths = TRUE)
    }

  }
}


# Downloads Geography and Data Tables

download_data <- function(geo_dir, endyear, span, geo_name) {

  if (!file.exists(geo_dir)) {

    dir.create(geo_dir, showWarnings = FALSE)

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    if (endyear >= 2009) {

      # data files for all seqs
      data_filename <- glue("{geo_name}_All_Geographies.zip")

      data_url <- glue("{base_url}/data/{span}_year_by_state/{data_filename}")
      data_file <- glue("{geo_dir}/{data_filename}")

      download_files(data_url, data_file)
      unzip_files(data_file, exdir = geo_dir)


    } else if (endyear <= 2008) {

      # get data files (including geographies)

      data_base_url <- dplyr::case_when(
        endyear == 2005 & geo_name == "UnitedStates" ~ as.character(glue("{base_url}/data/0UnitedStates")),
        endyear %in% 2005:2006                       ~ as.character(glue("{base_url}/data/{geo_name}")),
        endyear %in% 2007:2008                       ~ as.character(glue("{base_url}/data/{span}_year/{geo_name}"))
      )

      data_file_pattern <- dplyr::case_when(
        endyear %in% 2005      ~ "\\.2005-1yr",
        endyear %in% 2006:2008 ~ as.character(glue("{endyear}{span}"))
      )

      data_filenames <- scrape_filenames(data_base_url, data_file_pattern)

      data_urls <- glue('{data_base_url}/{data_filenames}')
      data_files <- glue("{geo_dir}/{data_filenames}")

      download_files(data_urls, data_files)
      # TODO:filter to only zipfiles
      unzip_files(data_files, exdir = geo_dir)

    }
  }
}
