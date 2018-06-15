#' Safely print any FontAwesome in R Markdown
#' @description This facilitates printing of
#' the FontAwesome icon within R Markdown.
#' @param x an object containing the class
#' \code{fontawesome}.
#' @keywords internal
#' @export
knit_print.fontawesome <- function(x, ...) {

  if (requireNamespace("knitr", quietly = TRUE)) {

    knitr::knit_print(as.character(x), ...)
  }
}
