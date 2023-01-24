#' Generate Font Awesome icons as SVGs
#'
#' Add one or more Font Awesome icons as SVGs contained within `<svg>...</svg>`.
#' We can optionally set certain style attributes. The `fa()` function can be
#' used directly within inline evaluations of R code in R Markdown documents.
#'
#' @param name The name of the Font Awesome icon. This could be as a short name
#'   (e.g., `"npm"`, `"drum"`, etc.), or, a full name (e.g., `"fab fa-npm"`,
#'   `"fas fa-drum"`, etc.). The names should correspond to current Version 6
#'   Font Awesome names. A list of short and full names can be accessed through
#'   the [fa_metadata()] function with `fa_metadata()$icon_names` and
#'   `fa_metadata()$icon_names_full`. If supplying a previous name associated
#'   with the icon, it will be internally translated to the current name and a
#'   Version 6 icon will be returned.
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
#' @param vertical_align The vertical alignment of the icon. By default, a
#'   length of `"-0.125em"` is used.
#' @param position The value for the `position` style attribute. By default,
#'   `"relative"` is used here.
#' @param title An option for populating the SVG `'title'` attribute, which
#'   provides on-hover text for the icon. By default, no title text is given to
#'   the icon. If `a11y == "semantic"` then title text will be
#'   automatically given to the rendered icon, however, providing text here
#'   will override that.
#' @param prefer_type Chooses the type of icon returned if: (1) providing a
#'   short name, and (2) that icon has both solid and regular types.
#'   For example, using `name = "address-book"` will result in two types of
#'   icons for an Address Book. By default, this preference is set to
#'   `"regular"` and the other option is `"solid"`.
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
fa <- function(
    name,
    fill = NULL,
    fill_opacity = NULL,
    stroke = NULL,
    stroke_width = NULL,
    stroke_opacity = NULL,
    height = NULL,
    width = NULL,
    margin_left = NULL,
    margin_right = NULL,
    vertical_align = NULL,
    position = NULL,
    title = NULL,
    prefer_type = c("regular", "solid"),
    a11y = c("deco", "sem", "none")
) {

  prefer_type <- match.arg(prefer_type)
  a11y <- match.arg(a11y)

  # Ensure that the `name` value passes basic validation checks
  check_name_vec(name = name)

  # Get the icon index value in `fa_tbl`
  idx <-
    get_icon_idx(
      name = name,
      prefer_type = prefer_type
    )

  # Extract the icon width, its label, and its path
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

    width <-
      paste0(
        round((icon_width / 512) * height_num, 2),
        attr(height_num, "unit")
      )

  } else if (is.null(height) && !is.null(width)) {

    height <-
      paste0(
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

  style_attr <-
    paste0(
      "height:", height, ";",
      "width:", width, ";",
      "vertical-align:", vertical_align %||% "-0.125em", ";",
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

# Retrieve the row indices within fa_tbl for all rows that match the icon name
# "name", using a variety of interpretations of that name (match on fa_tbl$name,
# match on fa_tbl$full_name, or using alias_tbl to translate the name).
#
# The returned value will be an integer vector, that may be:
# * Length 0: No results found
# * Length 1: Found exactly one type
# * Length >1: This icon comes in multiple types
get_icon_idx_all_types <- function(name) {

  # Attempt to match supplied `name` to short name in `fa_tbl$name`
  idx <- match(name, fa_tbl$full_name)
  if (!is.na(idx)) {
    return(idx)
  }

  # An `NA` value means that no match was made so there will be
  # another attempt to match against the full name in `fa_tbl$full_name`
  idx <- which(fa_tbl$name == name)
  if (length(idx) > 0) {
    return(idx)
  }

  # If still no match then there will be a final attempt to match against an
  # alias name in `alias_tbl$alias`; it's important to note that these alias
  # names are all short names (e.g., "vcard" is an alias to the canonical name
  # `address-card`);
  canonical_name <- alias_tbl[alias_tbl$alias == name, "name", drop = TRUE]
  if (length(canonical_name) > 0) {
    idx <- which(fa_tbl$name == canonical_name)
    return(idx)
  }

  # Nothing was found
  return(integer(0))
}

get_icon_idx <- function(
    name,
    prefer_type,
    fail_on_unknown_name = TRUE,
    msg_on_unknown_name = TRUE
) {

  # Get all fa_tbl indices that match--if any
  idx <- get_icon_idx_all_types(name = name)

  if (length(idx) == 0) {
    if (!fail_on_unknown_name) {

      if (msg_on_unknown_name) {
        message(
          "The `name` provided ('", name, "') does not correspond to a known icon"
        )
      }

      # This is only in the case of `fa_i()` where an unmatched name
      # should generate an <i> tag anyway
      return(NA_integer_)

    } else {

      stop(
        "The `name` provided ('", name, "') does not correspond to a known icon",
        call. = FALSE
      )
    }
  }

  # The possible use of a short name may result in a single match in the
  # `name` column of `fa_tbl`, no match at all, or multiple matches (usually
  # 2, but 3 in the case of `"font-awesome"`); resolve multiple matches with
  # the `prefer_type` value
  if (length(idx) > 1) {
    exact_match <- intersect(idx, which(fa_tbl$style == prefer_type))
    if (length(exact_match) == 1) {
      idx <- exact_match
    } else {
      # There were multiple icon types available, but not the one that was
      # indicated by `prefer_type`. Just use the first one.
      idx <- idx[1]
    }
  }

  idx
}

check_name_vec <- function(name) {

  if (!is.character(name)) {
    stop("A character vector should be supplied for `name`.", call. = FALSE)
  }

  if (length(name) != 1) {
    stop("The number of icons specified in `name` must be 1.", call. = FALSE)
  }
}
