#' Safely print any FontAwesome in R Markdown
#' @description This facilitates printing of
#' the FontAwesome icon within R Markdown.
#' @param x an object containing the class
#' `fontawesome`.
#' @keywords internal
knit_print.fontawesome <- function(x, ...) { # nocov start

  if (requireNamespace("knitr", quietly = TRUE)) {

    knitr::knit_print(as.character(x), ...)
  }
} # nocov end
