

# scrape census ftp webpage for list of files to download
scrape_filenames <- function(url, pattern = ".*") {
  xml2::read_html(url) %>%
    rvest::html_nodes("table td a") %>%
    rvest::html_attr("href") %>%
    .[-1] %>% # first always "parent dir"
    purrr::keep(stringr::str_detect, pattern = pattern)
}

# retries the download in case of error for specified number of times
download_retry <- function(..., times = 2) {
  while (times >=0) {
    ret <- try(utils::download.file(...), silent = TRUE)
    if(!methods::is(ret, 'try-error')) break
    times <- times - 1
    if (times >=0) message("error in download, retrying...")
  }
  if(methods::is(ret, 'try-error')) stop(ret[[1]], call. = FALSE)
}

# Downloads Seq/Table/Var Info
download_docs <- function(doc_dir, endyear, span) {

  if (!file.exists(doc_dir)) {

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    dir.create(doc_dir, showWarnings = FALSE)

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

      download_retry(docs_file_url, docs_file, mode = "wb", quiet = TRUE)

    } else if (endyear == 2005) {

      # seq/table info
      seq_filename <- "Chapter_5_tables_summary_list.xls"

      seq_url <- glue("{docs_base_url}/{seq_filename}")
      seq_file <- glue("{doc_dir}/{seq_filename}")

      download_retry(seq_url, seq_file, mode = "wb", quiet = TRUE)


      # also need table shells to build for table/vars
      shell_base_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2005"
      shell_filenames <- scrape_filenames(shell_base_url, "\\.xls$")

      shell_urls <- glue("{shell_base_url}/{shell_filenames}")
      shell_files <- glue("{doc_dir}/{shell_filenames}")

      purrr::walk2(shell_urls, shell_files, download_retry, mode = "wb", quiet = TRUE)

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

      download_retry(templates_url, templates_file, quiet = TRUE)

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

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    dir.create(geo_dir, showWarnings = FALSE)

    if (endyear >= 2009) {

      # data files for all seqs
      data_filename <- glue("{geo_name}_All_Geographies.zip")

      data_url <- glue("{base_url}/data/{span}_year_by_state/{data_filename}")
      data_file <- glue("{geo_dir}/{data_filename}")

      download_retry(data_url, data_file, quiet = TRUE)
      utils::unzip(data_file, exdir = geo_dir, junkpaths = TRUE)


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

      purrr::walk2(data_urls, data_files, download_retry, quiet = TRUE)
      purrr::walk(data_files, utils::unzip, exdir = geo_dir, junkpaths = TRUE)

    }
  }
}
