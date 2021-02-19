#' Print the fontawesome icon to the Viewer
#'
#' This function will show the fontawesome icon in the Viewer.
#'
#' @param x An agent object of class `fontawesome`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#'
#' @return No return value, called for printing to the Viewer.
#'
#' @keywords internal
#' @export
print.fontawesome <- function(x, view = interactive(), ...) {

  # nocov start

  dots <- list(...)

  html <-
    paste(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      x,
      collapse = "\n"
    )

  c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset=\"utf-8\">",
    "</head>",
    "<body>",
    html,
    "</body>",
    "</html>"
  ) %>%
    paste(collapse = "\n") %>%
    htmltools::HTML() %>%
    htmltools::html_print()

  message(x)

  # nocov end
}


