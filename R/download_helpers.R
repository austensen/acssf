
# TODO: delete zip files after extraction


# TODO: write bare-bones documentation for helper functions

# scrape census ftp webpage for list of files to download
scrape_filenames <- function(url, pattern = ".*") {

  xml2::read_html(url) %>%
    rvest::html_nodes("table td a") %>%
    rvest::html_attr("href") %>%
    .[-1] %>% # drop first element, always "parent dir"
    stringr::str_subset(pattern)

}

download_files <- function(urls, destfiles, ...) {

  purrr::walk2(urls, destfiles, curl::curl_download, ...)

}


# wrap unzip in walk to allow for unzipping multiple files, and preset arguments
unzip_files <- function(zip_files, exdir, files = NULL, junkpaths = TRUE, ...) {

  purrr::walk(zip_files, utils::unzip, exdir = exdir, junkpaths = TRUE, ...)

}

# Downloads Seq/Table/Var Info
download_docs <- function(docs_dir, endyear, span, geo_abb) {

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    docs_base_url <- glue("{base_url}/documentation")

  # Only one set of docs per year/span,
  # so If there already is a docs folder skip downloads
  if (!file.exists(docs_dir)) {

    dir.create(docs_dir, showWarnings = FALSE)


    # get Seq/Table/Var info
    if (endyear >= 2006) {

      docs_file_url <- dplyr::case_when(
        # weird problem of wrong files in FTP for 2006, found correct version in
        # different folder
        endyear == 2006                ~ "http://www2.census.gov/acs2006/merge_5_6_final.xls",
        endyear == 2007                ~ glue_chr("{docs_base_url}/{span}_year/merge_5_6_final.xls"),
        endyear == 2008                ~ glue_chr("{docs_base_url}/{span}_year/user_tools/merge_5_6.xls"),
        endyear == 2009 && span == 1L  ~ glue_chr("{docs_base_url}/{span}_year/user_tools/merge_5_6.xls"),
        endyear == 2009 && span == 5L  ~ glue_chr("{docs_base_url}/{span}_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls"),
        endyear %in% 2010:2012         ~ glue_chr("{docs_base_url}/{span}_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls"),
        endyear >=2013                 ~ glue_chr("{docs_base_url}/user_tools/ACS_{span}yr_Seq_Table_Number_Lookup.xls")
      )

      # standardize when saving local copy for easier lookup later
      docs_file <- glue("{docs_dir}/seq_table_lookup.xls")

      download_files(docs_file_url, docs_file, mode = "wb")

    } else if (endyear == 2005) {

      # seq/table info
      seq_filename <- "Chapter_5_tables_summary_list.xls"

      seq_url <- glue("{docs_base_url}/{seq_filename}")
      seq_file <- glue("{docs_dir}/{seq_filename}")

      download_files(seq_url, seq_file, mode = "wb")


      # also need table shells to build for table/vars
      shell_base_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2005"
      shell_filenames <- scrape_filenames(shell_base_url, "\\.xls$")

      shell_urls <- glue("{shell_base_url}/{shell_filenames}")
      shell_files <- glue("{docs_dir}/{shell_filenames}")

      download_files(shell_urls, shell_files)
    }

  }


  # for recent years get geography info
  if (endyear >= 2009) {

    geos_base_url <- dplyr::case_when(
      endyear <= 2012L              ~ glue_chr("{docs_base_url}/{span}_year/geography"),
      endyear == 2013L && span ==5L ~ glue_chr("{docs_base_url}/geography/{span}_year_Geo"),
      endyear >= 2014L && span ==5L ~ glue_chr("{docs_base_url}/geography/{span}yr_year_geo"),
      endyear >= 2013L && span ==1L ~ glue_chr("{docs_base_url}/geography")
    )

    geos_filename <- dplyr::case_when(
      span == 5L && endyear <= 2015L ~ glue_chr("{geo_abb}.xls"),
      span == 5L && endyear >= 2016L ~ glue_chr("{stringr::str_to_upper(geo_abb)}.xls"),
      span == 1L && endyear <= 2012L ~ "Mini_Geofile.xls",
      span == 1L && endyear == 2013L ~ "1_year_Mini_Geo.xls",
      span == 1L && endyear >= 2014L ~ "1_year_Mini_Geo.xlsx"
    )

    geos_url <- glue_chr("{geos_base_url}/{geos_filename}")

    geos_file <- glue_chr("{docs_dir}/{geos_filename}")

    # TODO: don't download if already there (wait until all bugs worked out)
    download_files(geos_url, geos_file)
  }
}


# Downloads Geography and Data Tables

download_data <- function(data_dir, endyear, span, geo_name) {

  # TODO: warn if no files are found on FTP (census error)
  # TODO: add warning if files already exist
  if (!file.exists(data_dir)) {

    dir.create(data_dir, showWarnings = FALSE)

    base_url <- glue("https://www2.census.gov/programs-surveys/acs/summary_file/{endyear}")

    if (endyear >= 2009) {

      if (span == 1L) {

        data_filenames <- dplyr::case_when(
          endyear == 2009 ~ glue_chr("{geo_name}.zip"),
          endyear > 2009  ~ glue_chr("{geo_name}_All_Geographies.zip")
        )

        data_files <- glue("{data_dir}/{data_filenames}")

        data_urls <- glue("{base_url}/data/{span}_year_by_state/{data_filenames}")

        download_files(data_urls, data_files)

        unzip_files(data_files, exdir = data_dir)

      } else if (span == 5L){


        data_dir_non_tract <- glue("{data_dir}/non_tract_blockgroup")
        data_dir_tract <- glue("{data_dir}/tract_blockgroup")

        dir.create(data_dir_non_tract, showWarnings = FALSE)
        dir.create(data_dir_tract, showWarnings = FALSE)

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

        unzip_files(data_files[[1]], exdir = data_dir_non_tract)
        unzip_files(data_files[[2]], exdir = data_dir_tract)
      }


    } else if (endyear <= 2008) {

      # get data files (including geographies)

      data_base_url <- dplyr::case_when(
        endyear == 2005 & geo_name == "UnitedStates" ~ glue_chr("{base_url}/data/0UnitedStates"),
        endyear %in% 2005:2006                       ~ glue_chr("{base_url}/data/{geo_name}"),
        endyear %in% 2007:2008                       ~ glue_chr("{base_url}/data/{span}_year/{geo_name}")
      )

      data_file_pattern <- dplyr::case_when(
        endyear %in% 2005      ~ "\\.2005-1yr",
        endyear %in% 2006:2008 ~ glue_chr("{endyear}{span}")
      )

      data_filenames <- scrape_filenames(data_base_url, data_file_pattern)

      data_urls <- glue('{data_base_url}/{data_filenames}')
      data_files <- glue("{data_dir}/{data_filenames}")

      download_files(data_urls, data_files)

      data_files %>%
        stringr::str_subset("\\.zip$") %>%
        unzip_files(exdir = data_dir)

    }
  }
}
