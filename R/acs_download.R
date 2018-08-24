#' Download ACS Summary File Data
#'
#' This function downloads American Community Survey (ACS) Summary File (SF)
#' data from the Census Bureau's FTP site.
#'
#' @param acs_dir \[`character(1)`]: The root directory in which all the ACS SF data will be saved.
#' @param endyear \[`integer(1)`]: The endyear of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span \[`integer(1)`]: The span of years for ACS estimates. ACS 1-year, and
#'   5-year surveys are supported.
#' @param geo \[`character(1)`]: The 2-letter abbreviation for the state for which data will be
#'   downloaded. For geogrpahies that do not nest within states, use `"US"`.
#' @param overwrite \[`logical(1)`]:  Whether existing versions of these files be overwriten.
#'   Defaults to `FALSE`.
#'
#' @export

acs_download <- function(acs_dir, endyear, span, geo, overwrite = FALSE) {

  # TODO: add support for taking vector of geos
  # TODO: when package ready, consider switch to https://github.com/ropensci/ftp
  # TODO: add messages for steps in process (too many separate processes for bar)

  dir.create(acs_dir, recursive = TRUE, showWarnings = FALSE)

  validate_args(
    endyear = endyear,
    span = span,
    overwrite = overwrite
  )

  geo_abb <- swap_geo_id(geo, "abb")
  geo_name <- swap_geo_id(geo_abb, "name")

  # capitalize "of" in 5-yr data
  if (span == 5L && geo_abb == "dc") {
    geo_name <- "DistrictOfColumbia"
  }

  raw_dir <- glue("{acs_dir}/Raw/{endyear}_{span}")

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  if (overwrite) {
    unlink(glue("{raw_dir}/_docs"), recursive = TRUE)
    unlink(glue("{raw_dir}/{geo_name}"), recursive = TRUE)
  } else if (file.exists(glue("{raw_dir}/{geo_name}"))) {
    warn_glue("{endyear} {span}-year data for {geo_name} already exists.")
    return(invisible(NULL))
  }


  # some downloads take a long time, temporarily change timeout (10min)
  op <- options(timeout = 600L)
  on.exit(options(op))

  download_docs(
    docs_dir = glue("{raw_dir}/_docs"),
    endyear = endyear,
    span = span,
    geo_abb = geo_abb
  )

  download_data(
    data_dir = glue("{raw_dir}/{geo_name}"),
    endyear = endyear,
    span = span,
    geo_name = geo_name
  )

  zip_files <- dir(raw_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  if (length(zip_files)) invisible(file.remove(zip_files))
}
