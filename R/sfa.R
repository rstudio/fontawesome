#' Generate a FontAwesome icon as an SVG for Shiny
#'
#' Add a FontAwesome icon to a Shiny app as SVG
#' contained within \code{<svg>...</svg>}. We
#' can optionally set certain style attributes.
#' This function differs from \code{fa()} in that
#' it is specialized for Shiny apps by preserving
#' the output as HTML.
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
#' sfa(name = "r-project")
#' @importFrom htmltools HTML
#' @export
sfa <- function(name,
                height = NULL,
                fill = NULL) {

  svg <- fa(name = name, height = height, fill = fill)

  htmltools::HTML(svg)
}
