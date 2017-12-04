


make_indicator_table <- function(acs_dir, out_dir = NULL, endyear, span, sum_level, .f = NULL) {

  if (is.null(.f)) {
    .f <- purrr::as_mapper(~return(.x))
  } else {
    .f <- purrr::as_mapper(.f)
  }

  # TODO: add sum_level to sum_level_name validator helper function
  sum_level_name <- switch(
    sum_level,
    "010" = "us",
    "310" = "cbsa",
    "160" = "place",
    "795" = "puma",
    "040" = "state",
    "050" = "county",
    "140" = "tract",
    "150" = "blockgroup"
  )

  if (is.null(out_dir)) {
    out_dir <- glue("{acs_dir}/Output")
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  out_file <- glue("{out_dir}/{sum_level_name}_{endyear}_{span}_indicators.csv")

  acs_cols <- readr::cols(
    .default = "d",
    endyear = "c",
    span = "c",
    geoid_full = "c",
    geoid = "c",
    sum_level = "c",
    geo_type = "c",
    geo_name = "c"
  )

  # TODO: consider writing to csv after each file, with map_dfr it might get too
  # large for processing blockgrous

  glue("{acs_dir}/Clean/{endyear}_{span}") %>%
    dir(pattern = glue("_{sum_level_name}_"), full.names = TRUE) %>%
    purrr::map_dfr(~{
      .x %>%
        readr::read_csv(col_types = acs_cols) %>%
        .f()
    }) %>%
    readr::write_csv(out_file, na = "")
}
