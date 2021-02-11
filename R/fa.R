#' Generate a FontAwesome icon as an SVG
#'
#' Add a FontAwesome icon as SVG contained within \code{<svg>...</svg>}. We can
#' optionally set certain style attributes. The \code{fa()} function can be used
#' directly within inline evaluations of R code (e.g., as \code{`r fa(...)`}) in
#' R Markdown documents.
#'
#' @param name the name of the FontAwesome icon.
#' @param height the absolute height (px) of the rendered SVG.
#' @param fill an option to change the fill color of the icon.
#'
#' @return an `svg` object that is the styled FontAwesome icon.
#'
#' @examples
#' # Emit a FontAwesome icon (`r-project`) as
#' # SVG within `svg` tags
#' fa(name = "r-project")
#'
#'
#' @export
fa <- function(name,
               height = NULL,
               fill = NULL) {

  # Create bindings for global variables
  full_name <- NULL

  if (name %in% fa_tbl$full_name) {

    svg <-
      fa_tbl[
        which(fa_tbl$full_name == name), ][1, 4]

  } else if (name %in% fa_tbl$name) {

    svg <-
      fa_tbl[
        which(fa_tbl$name == name), ][1, 4]

  } else {
    stop("This icon (`", name, "`) does not exist", call. = FALSE)
  }

  # Add `xmlns` attribute ---------------------------------------------------

  svg <- gsub("^<svg", "<svg xmlns=\"http://www.w3.org/2000/svg\"", svg)

  # Construct `style` attributes --------------------------------------------

  if (!is.null(height) || !is.null(fill)) {

    style <- "style=\""

    if (is.null(height)) {
      style <- paste0(style, "height:0.8em;top:0.04em;position:relative;")
    } else {
      style <- paste0(style, "height:", height, ";")
    }

    if (!is.null(fill)) {
      style <- paste0(style, "fill:", fill, ";")
    }

    style <- paste0(style, "\"")

    if (style != "style=\"\"") {
      svg <- gsub("^<svg", paste0("<svg ", style), svg)
    }
  }

  svg <- gsub("^<svg", '<svg class="rfa"', svg)

  svg <- htmltools::HTML(svg)

  class(svg) <- c("fontawesome", "svg", class(svg))

  svg
}
