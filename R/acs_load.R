#' Load all files created by acs_transform() into database table
#'
#' @param conn A [DBI::DBIConnection-class] object, as returned by
#'   [DBI::dbConnect()].
#' @param table_name A character string specifying the unquoted database table name.
#' @param acs_dir The root directory in which all the ACS
#'   data has been downloaded with [acs_download()]. Defaults to current working directory.
#'
#' @export
#'
acs_load <- function(conn, table_name, acs_dir =".") {

  # TODO: maybe add check that all files have the same schema before loading

  clean_dir <- glue("{acs_dir}/Clean")

  files <- fs::dir_ls(clean_dir, regexp = ".*\\.fst$", recursive = T)

  if (length(files) < 1) {
    stop_glue("There are no files in {clean_dir}
              First run `acs_transform()` to create files for upload.")
  }

  suppressMessages(DBI::dbExecute(conn, glue("DROP TABLE IF EXISTS {table_name}")))

  purrr::walk(files, function(path) {
    values <- fst::read_fst(path)
    DBI::dbWriteTable(conn, table_name, values, append = TRUE)
  })
}
