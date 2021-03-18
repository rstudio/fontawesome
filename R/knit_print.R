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
  if (knitr::pandoc_to(c("html", "html4", "html5", "slidy", "revealjs", "markdown"))) {
    # this formats support inline svg
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
      "fa-icon-",
      rlang::hash(x),
      formats$ext
    )
    if (!file.exists(icon_file)) {
      d <- dirname(icon_file)
      if (!dir.exists(d)) dir.create(d, recursive = TRUE)
      formats$renderer(
        charToRaw(as.character(x)),
        width = attr(x, "viewbox")[["width"]] / 2,
        height = attr(x, "viewbox")[["height"]] / 2,
        file = icon_file
      )
    }
    knitr::asis_output(
      sprintf("![](%s){height=%s width=%s}",
              icon_file,
              attr(x, "size")[["h"]],
              attr(x, "size")[["w"]]
      )
    )
  } else {
    warning("fontawesome does not support this output. Icon(s) will not show.",
            call. = FALSE)
    NULL
  }
  # nocov end
}
