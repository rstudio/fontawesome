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
      # create the icon in a tempfile and copy to fig.path for knitting
      tmp_pdf <- tempfile("fa-icon-", fileext = ".pdf")
      on.exit(unlink(tmp_pdf), add = TRUE)
      raw_fa <- charToRaw(as.character(x))
      rsvg::rsvg_pdf(raw_fa, file = tmp_pdf)
      knitr_fig_path <- options$fig.path
      pdf_icon <- file.path(knitr_fig_path, basename(tmp_pdf))
      if (!dir.exists(d <- dirname(pdf_icon))) dir.create(d, recursive = TRUE)
      file.copy(tmp_pdf, pdf_icon)
      knitr::asis_output(sprintf("\\includegraphics[height=0.8em]{%s}", pdf_icon))
    } else {
      warning("fontawesome does not supported this output. Icon(s) will not show.",
              call. = FALSE)
      ""
    }
  }
  # nocov end
}
