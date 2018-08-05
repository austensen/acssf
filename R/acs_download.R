#' Download ACS Summary File Data
#'
#' This function downloads American Community Survey (ACS) Summary File (SF)
#' data from the Census Bureau's FTP site.
#'
#' @param acs_dir The root directory in which all the ACS SF data will be saved.
#' @param endyear The endyear of the ACS sample. 2005 through 2016 are
#'   available.
#' @param span The span of years for ACS estimates. ACS contains 1-, 3-, and
#'   5-year surveys.
#' @param geo The 2-letter abbreviation for the state for which data will be
#'   downloaded. For geogrpahies that do not nest within states, use `"us"`.
#' @param overwrite Whether existing versions of these files be overwriten.
#'   Defaults to `FALSE`.
#'
#' @export

acs_download <- function(acs_dir, endyear, span, geo, overwrite = FALSE) {

  # TODO: test for 3- & 5-year, add support for taking vector of geos
  # TODO: when ready, consider switching using https://github.com/ropensci/ftp

  dir.create(acs_dir, recursive = TRUE, showWarnings = FALSE)

  validate_args(
    endyear = endyear,
    span = span,
    overwrite = overwrite
  )

  geo_name <- swap_geo_id(geo, "name")


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
    doc_dir = glue("{raw_dir}/_docs"),
    endyear = endyear,
    span = span
  )

  download_data(
    data_dir = glue("{raw_dir}/{geo_name}"),
    endyear = endyear,
    span = span,
    geo_name = geo_name
  )
}
