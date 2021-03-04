#' Generate a Font Awesome icon as an SVG
#'
#' Add a Font Awesome icon as SVG contained within `<svg>...</svg>`. We can
#' optionally set certain style attributes. The `fa()` function can be used
#' directly within inline evaluations of R code in R Markdown documents.
#'
#' @param name The name of the Font Awesome icon. This could be as a short name
#'   (e.g., `"npm"`, `"drum"`, etc.), or, a full name (e.g., `"fab fa-npm"`,
#'   `"fas fa-drum"`, etc.). The names should correspond to current Version 5
#'   Font Awesome names. A list of short and full names can be accessed through
#'   the [fa_metadata()] function with `fa_metadata()$icon_names` and
#'   `fa_metadata()$icon_names_full`. If supplying a Version 4 icon name, it
#'   will be internally translated to the Version 5 icon name and a Version 5
#'   icon will be returned. A data frame containing the short names that changed
#'   from version 4 (`v4_name`) to version 5 (`v5_name`) can be obtained by
#'   using `fa_metadata()$v4_v5_name_tbl`.
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
  } else if (name %in% fa_tbl$v4_name) {
    svg <- fa_tbl[fa_tbl$v4_name == name, ][1, "svg"]

    # Obtain the version 5 `name` and `full_name`
    # for messaging purposes
    v5_name <- fa_tbl[fa_tbl$v4_name == name, ][1, "name"]
    v5_name_full <- fa_tbl[fa_tbl$v4_name == name, ][1, "full_name"]

    # Warn that the v4 icon name should be changed to a v5 one
    warning(
      "The `name` provided ('", name ,"') is deprecated in Font Awesome v5:\n",
      "* please consider using '", v5_name, "' or '", v5_name_full, "' instead",
      call. = FALSE
    )

  } else {
    stop("This Font Awesome icon ('", name, "') does not exist", call. = FALSE)
  }

  match <- regexpr("viewBox=\".*?\"", svg)
  svg_viewbox <- regmatches(svg, match)

  viewbox_value <- gsub("viewBox=\"", "", svg_viewbox, fixed = TRUE)
  viewbox_value <- gsub("\"", "", viewbox_value, fixed = TRUE)

  svg_inner <- gsub("<svg.*?>", "", svg)
  svg_inner <- gsub("</svg>", "", svg_inner)

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
