#' Safely print any FontAwesome in R Markdown
#'
#' This facilitates printing of the FontAwesome icon within R Markdown.
#'
#' @param x An object of class `fontawesome`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.fontawesome <- function(x, ...) {

  # nocov start

  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_print(as.character(x), ...)
  }

  # nocov end
}
