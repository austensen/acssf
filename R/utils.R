# Adapted from Jenny Bryan's googlesheets package

cat_glue <- function(...) cat(glue::glue(..., .sep = "\n"))
msg_glue <- function(...) message(glue::glue(...))
warn_glue <- function(...) warning(glue::glue(...), call. = FALSE)
stop_glue <- function(...) stop(glue::glue(...), call. = FALSE)

# Some functions get confused by type = "glue"
glue_chr <- function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}") {
  as.character(glue::glue(..., .sep = .sep, .envir = .envir, .open = .open, .close = .close))
}

