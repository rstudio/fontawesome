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

  # TODO: use `knitr::pandoc_to()` in the future
  to <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # these formats support inline svg so use next method (i.e htmltools::knit_print.html)
  if (to %in% c("html", "html4", "html5", "slidy", "revealjs", "markdown")) {
    return(NextMethod())
  }

  # is the format supported for inline image file insertion ?
  formats <- switch(to,
    beamer = ,
    latex = list(ext = ".pdf", renderer = rsvg::rsvg_pdf),
    docx = list(ext = ".png", renderer = rsvg::rsvg_png),
    `markdown_github` = ,
    gfm = list(ext = ".svg", renderer = rsvg::rsvg_svg)
  )
  if (is.null(formats)) {
    warning("fontawesome does not support this output. Icon(s) will not show.",
      call. = FALSE
    )
    return(NULL)
  }

  # icon is written to file and inserted as image in the document
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Using fontawesome with non HTML output requires the rsvg package:\n",
      " * It can be installed with `install.packages(\"rsvg\")`.",
      call. = FALSE
    )
  }
  icon_file <- paste0(
    options$fig.path,
    "fa-icon-",
    rlang::hash(x),
    formats$ext
  )
  # icon is written once and file is reused if same icon is used several times
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
  # Pandoc Markdown syntax for images with link_attributes
  # https://pandoc.org/MANUAL.html#extension-link_attributes
  knitr::asis_output(
    sprintf(
      "![](%s){height=%s width=%s}",
      icon_file,
      attr(x, "size")[["h"]],
      attr(x, "size")[["w"]]
    )
  )
  # nocov end
}
