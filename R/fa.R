#' Generate a Font Awesome icon as an SVG
#'
#' Add a Font Awesome icon as SVG contained within `<svg>...</svg>`. We can
#' optionally set certain style attributes. The `fa()` function can be used
#' directly within inline evaluations of R code in R Markdown documents.
#'
#' @param name The name of the Font Awesome icon.
#' @param fill,fill_opacity The fill color of the icon can be set with `fill`.
#'   If not provided then the default value of `"currentColor"` is applied so
#'   that the SVG fill matches the color of the parent HTML element's `color`
#'   attribute. The opacity level of the SVG fill can be controlled with a
#'   decimal value between `0` and `1`.
#' @param stroke,stroke_width,stroke_opacity The stroke options allow for
#'   setting the color, width, and opacity of the SVG outline stroke. By
#'   default, the stroke width is very small at `"1px"` so a size adjustment
#'   with `"stroke_width"` can be useful. The `"stroke_opacity"` value can be
#'   any decimal values between `0` and `1` (bounds included).
#' @param height The absolute height of the rendered SVG. If nothing is provided
#'   then a default value of `"0.75em"` will be applied.
#' @param position The value for the `position` style attribute. By default,
#'   `"relative"` is used here.
#'
#' @return A `fontawesome` object that is the styled Font Awesome icon.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a Font Awesome SVG icon
#' fa(name = "r-project")
#'
#' }
#'
#' @export
fa <- function(name,
               fill = NULL,
               fill_opacity = NULL,
               stroke = NULL,
               stroke_width = NULL,
               stroke_opacity = NULL,
               height = NULL,
               position = NULL) {

  if (name %in% fa_tbl$full_name) {
    svg <- fa_tbl[fa_tbl$full_name == name, ][1, "svg"]
  } else if (name %in% fa_tbl$name) {
    svg <- fa_tbl[fa_tbl$name == name, ][1, "svg"]
  } else {
    stop("This icon (`", name, "`) does not exist", call. = FALSE)
  }

  match <- regexpr("viewBox=\".*?\"", svg)
  svg_viewbox <- regmatches(svg, match)

  viewbox_value <-
    svg_viewbox %>%
    gsub("viewBox=\"", "", ., fixed = TRUE) %>%
    gsub("\"", "", ., fixed = TRUE)

  svg_inner <-
    svg %>%
    gsub("<svg.*?>", "", .) %>%
    gsub("</svg>", "", .)

  svg <-
    htmltools::tags$svg(
      xmlns = "http://www.w3.org/2000/svg",
      viewBox = viewbox_value,
      class = "rfa",
      style = htmltools::css(
        height = height %||% "0.75em",
        fill = fill %||% "currentColor",
        fill_opacity = fill_opacity,
        stroke = stroke,
        stroke_width = stroke_width,
        stroke_opacity = stroke_opacity,
        position = position %||% "relative"
      ),
      htmltools::HTML(svg_inner)
    )

  class(svg) <- c("fontawesome", "svg", class(svg))

  svg
}
