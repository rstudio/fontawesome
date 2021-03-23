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
#'   using `fa_metadata()$v4_v5_name_tbl`. If multiple names are provided as
#'   a character vector, then multiple icons will be produced by concatenating
#'   together the SVG tags.
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
#' @param margin_right The length value for the margin that's right of the icon.
#'   By default, `"0.2rem"` is used.
#' @param position The value for the `position` style attribute. By default,
#'   `"relative"` is used here.
#' @param title An option for populating the SVG `'title'` attribute, which
#'   provides on-hover text for the icon. By default, no title text is given to
#'   the icon. If `a11y == "semantic"` then title text will be
#'   automatically given to the rendered icon, however, providing text here
#'   will override that.
#' @param a11y Cases that distinguish the role of the icon and inform which
#'   accessibility attributes to be used. Icons can either be `"desc"`
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
               margin_right = NULL,
               position = NULL,
               title = NULL,
               a11y = c("desc", "sem", "none")) {

  if (length(name) != 1) {
    stop("The number of icons specified in `name` must be 1.", call. = FALSE)
  }

  a11y <- match.arg(a11y, choices = c("desc", "sem", "none"))

  if (name %in% fa_tbl$name) {

    idx <- fa_tbl$name == name

    svg_list <- list(
      width = fa_tbl$width[idx][1],
      path  = fa_tbl$path[idx][1]
    )

  } else if (name %in% fa_tbl$full_name) {

    idx <- fa_tbl$full_name == name

    svg_list <- list(
      width = fa_tbl$width[idx][1],
      path  = fa_tbl$path[idx][1]
    )

  } else if (name %in% fa_tbl$v4_name) {

    idx <- fa_tbl$v4_name == name

    svg_list <- list(
      width = fa_tbl$width[idx][1],
      path  = fa_tbl$path[idx][1],
      name  = fa_tbl$name[idx][1],
      full_name  = fa_tbl$full_name[idx][1]
    )

    # Warn that the v4 icon name should be changed to a v5 one
    warning(
      "The `name` provided ('", name ,"') is deprecated in Font Awesome v5:\n",
      "* please consider using '", svg_list$name,
      "' or '", svg_list$full_name, "' instead",
      call. = FALSE
    )

  } else {
    stop("This Font Awesome icon ('", name, "') does not exist", call. = FALSE)
  }

  # Initialize vectors for extra SVG attributes and for the <title> tag
  extra_attrs <- ""
  title_tag <- ""

  # Generate the viewBox value through use of the only
  # changing value: the width
  viewbox_value <- c(`min-x` = 0, `min-y` = 0, width = svg_list$width, height = 512)

  # Generate the appropriate height and width attributes based on
  # user input and the SVG viewBox dimensions
  if (is.null(height) && is.null(width)) {
    # Case where height and width are not user-provided

    height_attr <- "1em"
    width_attr <- paste0(round(svg_list$width / 512, 2), "em")

  } else if (!is.null(height) && is.null(width)) {
    # Case where height is user-provided but `width` is not

    dim_list <- get_length_value_unit(css_length = height)

    height_attr <- height
    width_attr <-
      paste0(round((svg_list$width / 512) * dim_list$value, 2), dim_list$unit)

  } else if (is.null(height) && !is.null(width)) {
    # Case where width is user-provided but `height` is not

    dim_list <- get_length_value_unit(css_length = width)

    height_attr <-
      paste0(round(dim_list$value / (svg_list$width / 512), 2), dim_list$unit)
    width_attr <- width

  } else {
    # Case where both the `height` and `width` are provided

    # Invoke `get_length_value_unit()` to validate
    # the CSS length units in `height` and `width`
    get_length_value_unit(css_length = height)
    get_length_value_unit(css_length = width)

    extra_attrs <- "preserveAspectRatio=\"none\" "

    height_attr <- height
    width_attr <- width
  }

  # Generate accessibility attributes if either of
  # the "desc" or "sem" cases are chosen
  if (a11y == "none") {

    if (!is.null(title)) {
      title_tag <- paste0("<title>", htmlEscape(title), "</title>")
    }

  } else if (a11y == "desc") {

    extra_attrs <- paste0(extra_attrs, "aria-hidden=\"true\" role=\"img\" ")

    if (!is.null(title)) {
      title_tag <- paste0("<title>", htmlEscape(title), "</title>")
    }

  } else {
    # The 'semantic' case

    if (is.null(title)) {
      title <- fa_tbl$label[idx][1]
    }

    extra_attrs <-
      paste0(
        extra_attrs,
        "aria-label=\"",
        htmlEscape(title, attribute = TRUE), "\" ",
        "role=\"img\" "
      )

    title_tag <-
      paste0("<title>", htmlEscape(title), "</title>")
  }

  svg <-
    paste0(
      "<svg ",
      extra_attrs,
      "viewBox=\"", paste0(viewbox_value, collapse = " "), "\" " ,
      "style=\"",
      "height:", height_attr, ";",
      "width:", width_attr, ";",
      "vertical-align:-0.125em;",
      "margin-right:0.2em;",
      "font-size:inherit;",
      "fill:", fill %||% "currentColor", ";",
      "overflow:visible;",
      if (!is.null(fill_opacity)) paste0("fill-opacity:", fill_opacity, ";"),
      if (!is.null(stroke)) paste0("stroke:", stroke, ";"),
      if (!is.null(stroke_width)) paste0("stroke-width:", stroke_width, ";"),
      if (!is.null(stroke_opacity)) paste0("stroke-opacity:", stroke_opacity, ";"),
      "position:", position %||% "relative", ";",
      "\">",
      title_tag,
      svg_list$path,
      "</svg>"
    )

  svg <- HTML(svg)

  structure(svg,
            class = c("fontawesome", "svg", class(svg)),
            viewbox = viewbox_value,
            size = c(h = height_attr, w = width_attr)
  )
}

get_length_value_unit <- function(css_length) {

  if (!grepl("^[0-9]+[a-z]+$", css_length)) {

    stop(
      "Values provided to `height` and `width` must have a value followed by a length unit",
      call. = FALSE
    )
  }

  unit <- sub("^[0-9]+", "", css_length)

  if (!(unit %in% css_length_units)) {
    stop(
      "The provided CSS length unit is not valid.",
      call. = FALSE
    )
  }

  list(
    value = as.numeric(sub("[a-z]+$", "", css_length)),
    unit = unit
  )
}

css_length_units <-
  c(
    "cm", "mm", "in", "px", "pt", "pc", "em", "ex",
    "ch", "rem", "vw", "vh", "vmin", "vmax", "%"
  )
