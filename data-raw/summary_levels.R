

sum_level_info <- tibble::tribble(
  ~geo_type, ~sum_level, ~spans, ~years,
          "us", "010", c(1, 5), 2005:2017,
        "cbsa", "310", c(1, 5), 2005:2017,
       "place", "160", c(1, 5), 2005:2017,
        "puma", "795", c(1, 5), 2005:2017,
       "state", "040", c(1, 5), 2005:2017,
      "county", "050", c(1, 5), 2005:2017,
       "tract", "140", 5L,      2009:2017,
  "blockgroup", "150", 5L,      2009:2017
)

devtools::use_data(sum_level_info, overwrite = TRUE)
