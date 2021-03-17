#' Safely print any Font Awesome icon in R Markdown
#'
#' This facilitates printing of Font Awesome icons within R Markdown.
#'
#' @param x An object of class `fontawesome`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.fontawesome <- function(x, ..., options, inline = FALSE) {

  # nocov start

  if (requireNamespace("knitr", quietly = TRUE)) {
    if (knitr::is_html_output()) {
      NextMethod()
    } else if (knitr::pandoc_to(c("latex", "beamer", "docx"))) {
      if (!requireNamespace("rsvg", quietly = TRUE)) {
        stop("Using fontawesome with non HTML output requires the rsvg package:\n",
             " * It can be installed with `install.packages(\"rsvg\")`.",
             call. = FALSE)
      }
      formats <- switch(knitr::pandoc_to(),
                        beamer = ,
                        latex = list(ext = ".pdf", renderer = rsvg::rsvg_pdf),
                        docx = list(ext = ".png", renderer = rsvg::rsvg_png)
      )
      icon_file <- paste0(
        options$fig.path,
        basename(tempfile("fa-icon-", fileext = formats$ext))
      )
      if (!dir.exists(d <- dirname(icon_file))) dir.create(d, recursive = TRUE)
      raw_fa <- charToRaw(as.character(x))
      formats$renderer(raw_fa, file = icon_file)
      knitr::asis_output(sprintf("![](%s){%s}", icon_file, "height=1em"))
    } else {
      warning("fontawesome does not support this output. Icon(s) will not show.",
              call. = FALSE)
      NULL
    }
  }
  # nocov end
}
