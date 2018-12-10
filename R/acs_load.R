acs_load <- function(conn, table_name, acs_dir) {

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
