# Adapted from Jenny Bryan's googlesheets package

msg_glue <- function(..., .envir = parent.frame()) {
  message(glue::glue(..., .envir = .envir))
}

warn_glue <- function(...,.envir = parent.frame()) {
  warning(glue::glue(..., .envir = .envir), call. = FALSE)
}

stop_glue <- function(..., .envir = parent.frame()) {
  stop(glue::glue(..., .envir = .envir), call. = FALSE)
}


# Some functions get confused by type = "glue"
# TODO: test on new glue release, bug fixed in https://github.com/tidyverse/glue/issues/66
glue_chr <- function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}") {
  as.character(glue::glue(..., .sep = .sep, .envir = .envir, .open = .open, .close = .close))
}

