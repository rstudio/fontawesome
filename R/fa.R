#' Generate a FontAwesome icon as an SVG
#'
#' Add a FontAwesome icon as SVG contained within
#' \code{<svg>...</svg>}. We can optionally set
#' certain style attributes. The \code{fa()} function
#' can be used directly within inline evaluations
#' of R code (e.g., as \code{`r fa(...)`}) in R
#' Markdown documents.
#' @param name the name of the FontAwesome icon.
#' @param height the absolute height (px)
#' of the rendered SVG.
#' @param fill an option to change the fill
#' color of the icon.
#' @return a character object that is the styled
#' FontAwesome icon within \code{svg} tags.
#' @examples
#' # Emit a FontAwesome icon (`r-project`) as
#' # SVG within `svg` tags
#' fa(name = "r-project")
#' @importFrom htmltools HTML
#' @export
fa <- function(name,
               height = NULL,
               fill = NULL) {

  # Create bindings for global variables
  full_name <- NULL

  if (name %in% fontawesome:::fa_tbl$full_name) {

    svg <-
      fontawesome:::fa_tbl[
        which(fontawesome:::fa_tbl$full_name == name), ][1, 4]

  } else if (name %in% fontawesome:::fa_tbl$name) {

    svg <-
      fontawesome:::fa_tbl[
        which(fontawesome:::fa_tbl$name == name), ][1, 4]

  } else {
    return("")
  }

  # Construct `style` attributes --------------------------------------------

  style <- "style=\""

  if (is.null(height)) {
    style <- paste0(style, "height:0.8em;top:.04em;position:relative;")
  } else {
    style <- paste0(style, "height:", height, ";")
  }

  if (!is.null(fill)) {
    style <- paste0(style, "fill:", fill, ";")
  }

  style <- paste0(style, "\"")

  if (!grepl(style, pattern = "style=\"\"")) {

    svg <-
      gsub(
        pattern = "^<svg",
        replacement = paste0("<svg ", style),
        x = svg)
  }

  svg <- htmltools::HTML(svg)

  class(svg) <- c("fontawesome", class(svg))
  svg
}
