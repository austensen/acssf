# TODO: test for 3- & 5-year, add support for taking vector of geos

download_acs <- function(acs_dir, endyear, span, geo, overwrite) {

  dir.create(acs_dir, recursive = TRUE, showWarnings = FALSE)

  validate_args(
    endyear = endyear,
    span = span,
    overwrite = overwrite
  )

  geo_name <- swap_geo_name(geo)


  raw_dir <- glue("{acs_dir}/Raw/{endyear}_{span}")

  if (overwrite == TRUE) {
    file.remove(raw_dir)
  }

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)


  download_docs(
    doc_dir = glue("{raw_dir}/_docs"),
    endyear = endyear,
    span = span
  )


  download_data(
    geo_dir = glue("{raw_dir}/{geo_name}"),
    endyear = endyear,
    span = span,
    geo_name = geo_name
  )

}
