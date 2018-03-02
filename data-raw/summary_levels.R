

sum_level_info <- tibble::tribble(
  ~geo_type, ~sum_level, ~spans, ~endyears,
  "us", "010", "1, 5", "2005-2016",
  "cbsa", "310", "1, 5", "2005-2016",
  "place", "160", "1, 5", "2005-2016",
  "puma", "795", "1, 5", "2005-2016",
  "state", "040", "1, 5", "2005-2016",
  "county", "050", "1, 5", "2005-2016",
  "tract", "140", "5", "2009-2016",
  "blockgroup", "150", "5", "2009-2016"
)

devtools::use_data(sum_level_info, overwrite = TRUE)
