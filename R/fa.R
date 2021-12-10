#' Generate Font Awesome icons as SVGs
#'
#' Add one or more Font Awesome icons as SVGs contained within `<svg>...</svg>`.
#' We can optionally set certain style attributes. The `fa()` function can be
#' used directly within inline evaluations of R code in R Markdown documents.
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
#' @param height,width The height and width style attributes of the rendered
#'   SVG. If nothing is provided for `height` then a default value of `"1em"`
#'   will be applied. If a `width` isn't given, then it will be calculated in
#'   units of `"em"` on the basis of the icon's SVG `"viewBox"` dimensions.
#' @param margin_left,margin_right The length value for the margin that's either
#'   left or right of the icon. By default, `"auto"` is used for both
#'   properties. If space is needed on either side then a length of `"0.2em"` is
#'   recommended as a starting point.
#' @param position The value for the `position` style attribute. By default,
#'   `"relative"` is used here.
#' @param title An option for populating the SVG `'title'` attribute, which
#'   provides on-hover text for the icon. By default, no title text is given to
#'   the icon. If `a11y == "semantic"` then title text will be
#'   automatically given to the rendered icon, however, providing text here
#'   will override that.
#' @param a11y Cases that distinguish the role of the icon and inform which
#'   accessibility attributes to be used. Icons can either be `"deco"`
#'   (decorative, the default case) or `"sem"` (semantic). Using `"none"` will
#'   result in no accessibility features for the icon.
#'
#' @return A `fontawesome` object.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a Font Awesome SVG icon
#' fa(name = "r-project")
#'
#' }
#'
#' @import htmltools
#' @export
fa <- function(name,
               fill = NULL,
               fill_opacity = NULL,
               stroke = NULL,
               stroke_width = NULL,
               stroke_opacity = NULL,
               height = NULL,
               width = NULL,
               margin_left = NULL,
               margin_right = NULL,
               position = NULL,
               title = NULL,
               a11y = c("deco", "sem", "none")) {

  if (length(name) != 1) {
    stop("The number of icons specified in `name` must be 1.", call. = FALSE)
  }

  idx <- match(name, fa_tbl$name)
  if (is_na(idx)) {
    idx <- match(name, fa_tbl$full_name)
  }
  if (is_na(idx)) {
    idx <- match(name, fa_tbl$v4_name)
    if (is_na(idx)) {
      stop("This Font Awesome icon ('", name, "') does not exist", call. = FALSE)
    }
    warning(
      "The `name` provided ('", name, "') is deprecated in Font Awesome v5:\n",
      "* please consider using '", fa_tbl[idx, "name"],
      "' or '", fa_tbl[idx, "full_name"], "' instead",
      call. = FALSE
    )
  }

  icon_width <- fa_tbl$width[idx]
  icon_label <- fa_tbl$label[idx]
  icon_path <- fa_tbl$path[idx]

  # If both height and width are specified, don't preserve aspect ratio
  svg_attrs <- ""
  if (!is.null(height) && !is.null(width)) {
    svg_attrs <- paste0(svg_attrs, 'preserveAspectRatio="none" ')
  }

  # Validate the CSS length unit on height/width (if specified),
  # and return a number with the unit attached as an attribute
  height_num <- parse_length_unit(height)
  width_num <- parse_length_unit(width)

  # Fill in height/width defaults
  if (is.null(height) && is.null(width)) {

    height <- "1em"
    width <- paste0(round(icon_width / 512, 2), "em")

  } else if (!is.null(height) && is.null(width)) {

    width <- paste0(
      round((icon_width / 512) * height_num, 2),
      attr(height_num, "unit")
    )

  } else if (is.null(height) && !is.null(width)) {

    height <- paste0(
      round(width_num / (icon_width / 512), 2),
      attr(width_num, "unit")
    )

  }

  # Generate accessibility attributes if either of
  # the "deco" or "sem" cases are chosen
  a11y <- match.arg(a11y)
  if (a11y == "deco") {
    svg_attrs <- paste0(svg_attrs, 'aria-hidden="true" role="img" ')
  } else if (a11y == "sem") {
    title <- title %||% icon_label
    svg_attrs <- paste0(
      svg_attrs, sprintf('aria-label="%s" role="img" ', htmlEscape(title, attribute = TRUE))
    )
  }

  # Generate the viewBox value through use of the only
  # changing value: the width
  viewbox <- c(`min-x` = 0, `min-y` = 0, width = icon_width, height = 512)

  style_attr <- paste0(
    "height:", height, ";",
    "width:", width, ";",
    "vertical-align:-0.125em;",
    "margin-left:", margin_left %||% "auto", ";",
    "margin-right:", margin_right %||% "auto", ";",
    "font-size:inherit;",
    "fill:", fill %||% "currentColor", ";",
    "overflow:visible;",
    if (!is.null(fill_opacity)) paste0("fill-opacity:", fill_opacity, ";"),
    if (!is.null(stroke)) paste0("stroke:", stroke, ";"),
    if (!is.null(stroke_width)) paste0("stroke-width:", stroke_width, ";"),
    if (!is.null(stroke_opacity)) paste0("stroke-opacity:", stroke_opacity, ";"),
    "position:", position %||% "relative", ";"
   )

  svg_attrs <- paste0(
    svg_attrs, sprintf(
      'viewBox="%s" style="%s"',
      paste0(viewbox, collapse = " "),
      style_attr
    )
  )

  svg <- HTML(sprintf(
      '<svg %s>%s<path d="%s"/></svg>',
      svg_attrs,
      if (is.null(title)) "" else paste0("<title>", htmlEscape(title), "</title>"),
      icon_path
  ))

  structure(
    svg, viewbox = viewbox,
    size = c(h = height, w = width),
    class = c("fontawesome", "svg", class(svg))
  )
}

parse_length_unit <- function(css_length) {

  if (is.null(css_length)) {
    return(NULL)
  }

  if (!grepl("^^[0-9]*\\.?[0-9]+[a-z]+$", css_length)) {

    stop(
      "Values provided to `height` and `width` must have a numerical value ",
      "followed by a length unit.",
      call. = FALSE
    )
  }

  unit <- gsub("[0-9\\.]+?", "", css_length)

  if (!(unit %in% css_length_units)) {
    stop(
      "The provided CSS length unit is not valid.",
      call. = FALSE
    )
  }

  value <- as.numeric(gsub("[a-z]+$", "", css_length))
  attr(value, "unit") <- unit
  value
}

css_length_units <- c(
  "cm", "mm", "in", "px", "pt", "pc", "em", "ex",
  "ch", "rem", "vw", "vh", "vmin", "vmax", "%"
)
