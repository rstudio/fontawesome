#' @export
print.svg <- function(x, ..., view = interactive()) {

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
}
