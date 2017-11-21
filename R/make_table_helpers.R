


get_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb, sum_level) {

  if (file.exists(glue("{docs_dir}/geos_table.rds"))) {

    geos_table <- readr::read_rds(glue("{docs_dir}/geos_table.rds"))

  } else {

    geos_table <- make_geos_table(
      data_dir = data_dir,
      docs_dir = docs_dir,
      endyear = endyear,
      span = span,
      geo_abb = geo_abb)
  }

  dplyr::filter(geos_table, sumlevel == sum_level)
}

make_geos_table <- function(data_dir, docs_dir, endyear, span, geo_abb) {


  if (endyear >= 2009) {

    geo_col_names <- glue("{docs_dir}/{endyear}_SFGeoFileTemplate.xls") %>%
      readxl::read_excel(n_max = 0) %>%
      names() %>%
      stringr::str_to_lower()

    if (endyear == 2010 && span == 5) {

      geos_table_raw <- readr::read_table(
        glue("{data_dir}/g{endyear}{span}{geo_abb}.txt"),
        col_names = geo_col_names,
        col_types = readr::cols(.default = "c")
      )
    } else {

      geos_table_raw <- readr::read_csv(
        glue("{data_dir}/g{endyear}{span}{geo_abb}.csv"),
        col_names = geo_col_names,
        col_types = readr::cols(.default = "c")
      )
    }

  } else {

    # no template files in these years
    # so need to get col names and positions from sas programs
    geo_fwf_cols <- get_geo_fwf_cols(endyear)

    geos_table_raw <- readr::read_fwf(
      glue("{data_dir}/g{endyear}{span}{geo_abb}.txt"),
      col_positions = geo_fwf_cols,
      col_types = readr::cols(.default = "c")
    )
  }

  geos_table_raw %>%
    dplyr::select(c("logrecno", "geoid", "sumlevel")) %>%
    readr::write_rds(glue("{docs_dir}/geos_table.rds"))
}



get_geo_fwf_cols <- function(endyear) {

  if (endyear == 2005L) {
    # taken from:
    # https://www2.census.gov/programs-surveys/acs/summary_file/2005/documentation/0sas_exampleprograms/acssfgeo.sas

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      sumlevel = c(9, 11),
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
      geoid = c(111, 150),
      name = c(151, NA)
    )
  } else if (endyear %in% 2006:2007) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2007/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    # are no files for 2006, so use 2005 positions

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      sumlevel = c(9, 11),
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
      geoid = c(176, 215),
      name = c(216, NA)
    )
  } else if (endyear == 2008L) {

    # https://www2.census.gov/programs-surveys/acs/summary_file/2008/documentation/1_year/0sasexampleprograms/acssfgeo.sas

    readr::fwf_cols(
      # fileid = c(1, 6),
      # stusab = c(7, 8),
      sumlevel = c(9, 11),
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
      geoid = c(176, 215),
      name = c(216, NA)
    )
  }
}
