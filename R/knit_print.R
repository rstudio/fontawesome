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
    } else if (knitr::is_latex_output()) {
      if (!requireNamespace("rsvg", quietly = TRUE)) {
        stop("Using fontawesome in PDF output requires the rsvg package:\n",
             " * It can be installed with `install.packages(\"rsvg\")`.",
             call. = FALSE)
      }
      pdf_icon <- paste0(
        options$fig.path,
        basename(tempfile("fa-icon-", fileext = ".pdf"))
      )
      if (!dir.exists(d <- dirname(pdf_icon))) dir.create(d, recursive = TRUE)
      raw_fa <- charToRaw(as.character(x))
      rsvg::rsvg_pdf(raw_fa, file = pdf_icon)
      knitr::asis_output(sprintf("![](%s){%s}", pdf_icon, "height=0.8em"))
    } else {
      warning("fontawesome does not supported this output. Icon(s) will not show.",
              call. = FALSE)
      ""
    }
  }
  # nocov end
}
