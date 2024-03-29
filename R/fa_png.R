#' Create a PNG version of a Font Awesome icon
#'
#' Get a Font Awesome icon as a PNG file. We can optionally set the fill
#' attribute before writing the PNG. Additionally, there is control over the
#' output width and height (usually, icons are 512 by 512 pixels). Please note
#' that this function requires that the **rsvg** is installed on the system.
#' Attempting to use `fa_png()` without **rsvg** available will result in an
#' error message.
#'
#' @param name The name of the Font Awesome icon.
#' @param file the path to the output file. If `NULL`, then filename will take
#'   the short name of the icon and a `.png` extension will be applied.
#' @param fill,fill_opacity The fill color of the icon can be set with `fill`.
#'   If not provided then the default fill color will be black. The opacity
#'   level of the fill color can be controlled with a decimal value between `0`
#'   and `1`.
#' @param stroke,stroke_width,stroke_opacity The stroke options allow for
#'   setting the color, width, and opacity of the outline stroke. By default,
#'   the stroke width is very small at `"1px"` so a size adjustment with
#'   `"stroke_width"` can be useful. The `"stroke_opacity"` value can be any
#'   decimal values between `0` and `1` (bounds included).
#' @param height,width The output height and width of the rendered PNG. If
#'   nothing is provided then the output dimensions will match that of the input
#'   SVG viewBox.
#' @param prefer_type Chooses the type of icon returned if: (1) providing a
#'   short name, and (2) that icon has both solid and regular types.
#'   For example, using `name = "address-book"` will result in two types of
#'   icons for an Address Book. By default, this preference is set to
#'   `"regular"` and the other option is `"solid"`.
#'
#' @return A PNG file written to disk.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a Font Awesome SVG icon as a
#' # PNG file on disk
#' fa_png(name = "r-project")
#'
#' }
#'
#' @export
fa_png <- function(
    name,
    file = NULL,
    fill = NULL,
    fill_opacity = NULL,
    stroke = NULL,
    stroke_width = NULL,
    stroke_opacity = NULL,
    height = NULL,
    width = NULL,
    prefer_type = c("regular", "solid")
) {

  # nocov start

  # The `rsvg` package is required for this function; stop
  # function if it's not installed
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop(
      "Use of the `fa_png()` function requires the rsvg package:\n",
      " * It can be installed with `install.packages(\"rsvg\")`.",
      call. = FALSE
    )
  }

  # Obtain the SVG
  svg <-
    fa(
      name = name,
      fill = fill,
      fill_opacity = fill_opacity,
      stroke = stroke,
      stroke_width = stroke_width,
      stroke_opacity = stroke_opacity,
      prefer_type = prefer_type
    )

  # If `file` is `NULL` then construct a suitable
  # filename based on the icon's short name
  if (is.null(file)) {
    if (name %in% fa_tbl$full_name) {
      short_name <- fa_tbl[fa_tbl$full_name == name, ][1, "name"]
    } else if (name %in% fa_tbl$name) {
      short_name <- name
    }
    file <- paste0(short_name, ".png")
  }

  rsvg::rsvg_png(
    svg = charToRaw(as.character(svg)),
    file = file,
    width = width,
    height = height
  )

  # nocov end
}
