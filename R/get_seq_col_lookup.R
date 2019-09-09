
get_seq_col_lookup <- function(docs_dir, year) {

  # If it's already been created just load it
  if (fs::file_exists(glue("{docs_dir}/seq_col_lookup.rds"))) {
    readr::read_rds(glue("{docs_dir}/seq_col_lookup.rds"))
  } else {
    make_seq_col_lookup(docs_dir, year)
  }
}


make_seq_col_lookup <- function(docs_dir, year) {

  # If it's already been created just load it
  if (fs::file_exists(glue("{docs_dir}/seq_col_lookup.rds"))) {
    return(readr::read_rds(glue("{docs_dir}/seq_col_lookup.rds")))
  }

  if (year == 2005) {

    # There's a typo in shell table name from Census
    if (fs::file_exists(glue("{raw_dir}/_docs/B19113%20.xls"))) {
      fs::file_move(
        glue("{raw_dir}/_docs/B19113%20.xls"),
        glue("{raw_dir}/_docs/B19113.xls")
      )
    }

    # For a table ID (eg. "B01001") get a vector of all table_vars (eg.
    # "b01001_001") TODO: this doesn't work because of the B/C tables. C tables
    # are included in the B table shells, but they aren't always in the same seq
    # numbers. instead of this, loop over all shell tables in directory, get the
    # table vars for them all, then load the seq/table data and inner_join
    get_2005_table_vars <- function(table_id) {
      if (!file.exists(glue("{raw_dir}/_docs/{table_id}.xls"))) return(NA)
      glue("{raw_dir}/_docs/{table_id}.xls") %>%
        readxl::read_xls() %>%
        dplyr::filter(stringr::str_detect({{"Line Number"}}, "^\\d+$")) %>%
        dplyr::mutate(
          table = stringr::str_to_lower({{"Table ID"}}),
          row = stringr::str_pad({{"Line Number"}}, 3, "left", "0"),
          table_var = stringr::str_c({{"table"}}, "_", {{"row"}})
        ) %>%
        dplyr::pull({{"table_var"}})
    }

    # Need to get the seq/table correspondance from this fine, but get the
    # table_vars from shell tables
    vars_raw <- glue("{docs_dir}/Chapter_5_tables_summary_list.xls") %>%
      readxl::read_excel() %>%
      dplyr::transmute(
        table = {{"Table ID"}},
        seq = as.integer({{"Sequence Number"}})
      ) %>%
      dplyr::filter(
        !is.na({{"table"}}),
        # for some reason there are no data files for these seq
        !{{"seq"}} %in% 139:148,
        {{"seq"}} <= 150,
        # C tables have row info inside same-numer B table .xls
        stringr::str_detect({{"table"}}, "^B"),
        # Some random tables missing from xls files:
        !{{"table"}} %in% c("B08134", "B992523")
      ) %>%
      dplyr::transmute(
        seq = stringr::str_pad({{"seq"}}, 7, "left", "0"),
        table_var = purrr::map({{"table"}}, get_2005_table_vars)
      ) %>%
      tidyr::unnest({{"table_var"}}) %>%
      # The table shells don't match the data files in some cases. they list more
      # rows that are actually in the table. For example it says there are 10 rows
      # for c02005 but there are really only 9 (confirmed on FactFinder)
      dplyr::filter(
        !{{"table_var"}} %in% c("c02005_010"),
        !stringr::str_detect({{"table_var"}}, "pr")
      ) %>%
      dplyr::group_by({{"seq"}}) %>%
      dplyr::summarise(table_var = list({{"table_var"}}))

    seq_col_lookup <- purrr::set_names(vars_raw[["table_var"]], vars_raw[["seq"]])

  } else {
    vars_raw <- glue("{docs_dir}/seq_table_lookup.xls") %>%
      readxl::read_excel(col_types = "text") %>%
      # column name formats differ, but order is consistent
      dplyr::select(
        table = 2,
        seq = 3,
        line_num = 4
      ) %>%
      # remove extra rows (table fillers) to avoid duplicate table_vars
      dplyr::filter(
        !is.na({{"line_num"}}),
        {{"line_num"}} != "0",
        !stringr::str_detect({{"line_num"}}, "\\.")
      ) %>%
      dplyr::transmute(
        table = stringr::str_to_lower({{"table"}}),
        var = stringr::str_pad({{"line_num"}}, 3, "left", "0"),
        table_var = stringr::str_c({{"table"}}, "_", {{"var"}}),
        seq = stringr::str_c(stringr::str_pad({{"seq"}}, 4, "left", "0"), "000")
      )

    # create a named list of table_vars (names are seq numbers)
    seq_col_lookup <- split(vars_raw[["table_var"]], vars_raw[["seq"]])
  }

  readr::write_rds(seq_col_lookup, glue("{docs_dir}/seq_col_lookup.rds"))
}



