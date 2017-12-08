



# TODO: decide if its worth having a variable variable lookup table at all. If
# so what should it include and how should it be laid out. Could have one with
# universe, definition, and row descriptions. Is this the best way to look up
# variables?


# Vars --------------------------------------------------------------------


get_seq_col_lookup <- function(docs_dir, endyear) {

  if (file.exists(glue("{docs_dir}/seq_col_lookup.rds"))) {

    readr::read_rds(glue("{docs_dir}/seq_col_lookup.rds"))

  } else {

    make_seq_col_lookup(docs_dir, endyear)
  }
}


make_seq_col_lookup <- function(docs_dir, endyear) {

  if (endyear == 2005) {
    # TODO: add code to parse multiple seq tables for seq/vars
    stop_glue("Need to add 2005 functionality")

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
        !is.na(line_num),
        line_num != "0",
        !stringr::str_detect(line_num, "\\.")
      ) %>%
      dplyr::transmute(
        table = stringr::str_to_lower(table),
        var = stringr::str_pad(line_num, 3, "left", "0"),
        table_var = stringr::str_c(table, "_", var),
        seq = stringr::str_c(stringr::str_pad(seq, 4, "left", "0"), "000")
      )

    # create a named list of table_vars (names are seq numbers)
    seq_col_lookup <- split(vars_raw[["table_var"]], vars_raw[["seq"]])
  }


  readr::write_rds(seq_col_lookup, glue("{docs_dir}/seq_col_lookup.rds"))

}



# Geos --------------------------------------------------------------------

get_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb, .sum_level) {

  # TODO: once al bugs fixed, check if file exists before making

  # if (file.exists(glue("{data_dir}/geos_table.rds"))) {
  #
  #   geos_table <- readr::read_rds(glue("{data_dir}/geos_table.rds"))
  #
  # } else {

    geos_table <- make_geos_table(
      data_dir = data_dir,
      docs_dir = docs_dir,
      endyear = endyear,
      span = span,
      geo_abb = geo_abb)
  # }

  dplyr::filter(geos_table, sum_level == .sum_level)
}


make_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb) {

  if (span == 5L) {

    geo_abb <- dplyr::case_when(
      endyear >= 2015L ~ geo_abb,
      endyear >= 2016L ~ stringr::str_to_upper(geo_abb)
    )

    geos_table_raw <- glue("{docs_dir}/{geo_abb}.xls") %>%
      readxl::read_xls(col_types = "text", skip = 1) %>%
      dplyr::select(2:4) %>%
      purrr::set_names(c("logrecno", "geoid_full", "geo_name"))

  } else if (span == 1L) {

    if (endyear <= 2008L) {

      # no template files in these years
      # so need to get col names and positions from sas programs
      geo_fwf_cols <- get_geo_fwf_cols(endyear)

      geos_table_raw <- readr::read_fwf(
        glue("{data_dir}/g{endyear}{span}{geo_abb}.txt"),
        col_positions = geo_fwf_cols,
        col_types = readr::cols(.default = "c")
      )

    } else if (endyear <=2012L) {


      geos_filename <- dplyr::case_when(
        endyear <= 2012L ~ "Mini_Geofile.xls",
        endyear == 2013L ~ "1_year_Mini_Geo.xls"
      )

      geo_abb <- dplyr::case_when(
        endyear <= 2010L       ~ stringr::str_to_upper(geo_abb),
        endyear %in% 2011:2012 ~ geo_abb
      )

      geos_table_raw <- glue("{docs_dir}/{geos_filename}") %>%
        readxl::read_xls(sheet = geo_abb, col_types = "text", skip = 1) %>%
        dplyr::select(1:3) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))

    } else if (endyear >= 2013L) {

      geos_filename <- dplyr::case_when(
        endyear == 2013L ~ "1_year_Mini_Geo.xls", # actually an xlsx file, saved as ".xls"
        endyear >= 2014L ~ "1_year_Mini_Geo.xlsx"
      )

      geo_abb <- dplyr::case_when(
        endyear == 2013L ~ stringr::str_to_upper(geo_abb),
        endyear == 2014L ~ geo_abb,
        endyear >= 2015L ~ stringr::str_to_upper(geo_abb)
      )

      geos_table_raw <- glue("{docs_dir}/{geos_filename}") %>%
        readxl::read_xlsx(sheet = geo_abb, col_types = "text", skip = 1) %>%
        dplyr::select(1:3) %>%
        purrr::set_names(c("logrecno", "geoid_full", "geo_name"))

    }
  }

  geos_table_raw %>%
    dplyr::mutate(
      sum_level = stringr::str_sub(geoid_full, 1, 3),
      geoid = stringr::str_extract(geoid_full, "\\d+$")) %>%
    readr::write_rds(glue("{data_dir}/geos_table.rds"))
}



get_geo_fwf_cols <- function(endyear) {

  if (endyear == 2005L) {
    # taken from:
    # https://www2.census.gov/programs-surveys/acs/summary_file/2005/documentation/0sas_exampleprograms/acssfgeo.sas

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      # sumlevel = c(9, 11),
      # component = c(12, 13),
      logrecno = c(14, 20),
      # us = c(21, 21),
      # region = c(22, 22),
      # division = c(23, 23),
      # st = c(24, 25),
      # cty = c(26, 28),
      # mcd = c(29, 31),
      # pl = c(32, 35),
      # aindn = c(36, 39),
      # cbsa = c(40, 44),
      # metdiv = c(45, 49),
      # csa = c(50, 52),
      # cnecta = c(53, 55),
      # necta = c(56, 60),
      # nectadiv = c(61, 65),
      # ua = c(66, 70),
      # cd2000 = c(71, 72),
      # bst = c(73, 74),
      # puma5 = c(75, 79),
      # sdelm = c(80, 84),
      # sdsec = c(85, 89),
      # sduni = c(90, 94),
      # ur = c(95, 95),
      # memi = c(96, 96),
      # memir = c(97, 97),
      # pci = c(98, 98),
      # cd1990 = c(99, 100),
      # fipsmcd = c(101, 105),
      # fipspl = c(106, 110),
      geoid_full = c(111, 150),
      geo_name = c(151, NA)
    )
  } else if (endyear %in% 2006:2007) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2007/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    # are no files for 2006, so use 2005 positions

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      # sumlevel = c(9, 11),
      # component = c(12, 13),
      logrecno = c(14, 20),
      # us = c(21, 21),
      # region = c(22, 22),
      # division = c(23, 23),
      # statece = c(24, 25),
      # state = c(26, 27),
      # county = c(28, 30),
      # cousub = c(31, 35),
      # place = c(36, 40),
      # tract = c(41, 46),
      # blkgrp = c(47, 47),
      # concit = c(48, 52),
      # aianhh = c(53, 56),
      # aianhhfp = c(57, 61),
      # aihhtli = c(62, 62),
      # aitsce = c(63, 65),
      # aits = c(66, 70),
      # anrc = c(71, 75),
      # cbsa = c(76, 80),
      # csa = c(81, 83),
      # metdiv = c(84, 88),
      # macc = c(89, 89),
      # memi = c(90, 90),
      # necta = c(91, 95),
      # cnecta = c(96, 98),
      # nectadiv = c(99, 103),
      # ua = c(104, 108),
      # uacp = c(109, 113),
      # cdcurr = c(114, 115),
      # sldu = c(116, 118),
      # sldl = c(119, 121),
      # vtd = c(122, 127),
      # zcta3 = c(128, 130),
      # zcta5 = c(131, 135),
      # submcd = c(136, 137),
      # sdelm = c(138, 142),
      # sdsec = c(143, 147),
      # sduni = c(148, 152),
      # ur = c(153, 153),
      # pci = c(154, 154),
      # taz = c(155, 160),
      # uga = c(161, 165),
      # puma5 = c(166, 170),
      # puma1 = c(171, 175),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  } else if (endyear == 2008L) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2008/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      # sumlevel = c(9, 11),
      # component = c(12, 13),
      logrecno = c(14, 20),
      # us = c(21, 21),
      # region = c(22, 22),
      # division = c(23, 23),
      # statece = c(24, 25),
      # state = c(26, 27),
      # county = c(28, 30),
      # cousub = c(31, 35),
      # place = c(36, 40),
      # tract = c(41, 46),
      # blkgrp = c(47, 47),
      # concit = c(48, 52),
      # aianhh = c(53, 56),
      # aianhhfp = c(57, 61),
      # aihhtli = c(62, 62),
      # aitsce = c(63, 65),
      # aits = c(66, 70),
      # anrc = c(71, 75),
      # cbsa = c(76, 80),
      # csa = c(81, 83),
      # metdiv = c(84, 88),
      # macc = c(89, 89),
      # memi = c(90, 90),
      # necta = c(91, 95),
      # cnecta = c(96, 98),
      # nectadiv = c(99, 103),
      # ua = c(104, 108),
      # uacp = c(109, 113),
      # cdcurr = c(114, 115),
      # sldu = c(116, 118),
      # sldl = c(119, 121),
      # vtd = c(122, 127),
      # zcta3 = c(128, 130),
      # zcta5 = c(131, 135),
      # submcd = c(136, 137),
      # sdelm = c(138, 142),
      # sdsec = c(143, 147),
      # sduni = c(148, 152),
      # ur = c(153, 153),
      # pci = c(154, 154),
      # taz = c(155, 160),
      # uga = c(161, 165),
      # puma5 = c(166, 170),
      # puma1 = c(171, 175),
      geoid_full = c(176, 215),
      geo_name = c(216, NA)
    )
  }
}
